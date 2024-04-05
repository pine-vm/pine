module Pine exposing
    ( ConditionalExpressionStructure
    , EvalContext
    , Expression(..)
    , KernelApplicationExpressionStructure
    , KernelFunction
    , ParseAndEvalExpressionStructure
    , PathDescription(..)
    , Value(..)
    , addToEnvironment
    , bigIntFromBlobValue
    , bigIntFromUnsignedBlobValue
    , bigIntFromValue
    , blobValueFromBigInt
    , displayStringFromPineError
    , emptyEvalContext
    , encodeExpressionAsValue
    , environmentExpr
    , environmentFromDeclarations
    , evaluateExpression
    , falseValue
    , intFromValue
    , kernelFunction_Negate
    , mapFromListValueOrBlobValue
    , parseExpressionFromValue
    , stringAsValue_Function
    , stringAsValue_List
    , stringAsValue_Literal
    , stringFromListValue
    , stringFromValue
    , trueValue
    , valueFromBigInt
    , valueFromBool
    , valueFromChar
    , valueFromContextExpansionWithName
    , valueFromInt
    , valueFromString
    )

import BigInt
import Common
import Dict
import Hex


type Expression
    = LiteralExpression Value
    | ListExpression (List Expression)
    | ParseAndEvalExpression ParseAndEvalExpressionStructure
    | KernelApplicationExpression KernelApplicationExpressionStructure
    | ConditionalExpression ConditionalExpressionStructure
    | EnvironmentExpression
    | StringTagExpression String Expression


type alias ParseAndEvalExpressionStructure =
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
    ListValue
        (List.map valueFromContextExpansionWithName declarations)


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
    { environment = listValue_Empty }


evaluateExpression : EvalContext -> Expression -> Result (PathDescription String) Value
evaluateExpression context expression =
    case expression of
        LiteralExpression value ->
            Ok value

        ListExpression listElements ->
            case
                Common.resultListIndexedMapCombine
                    (\listElementIndex listElement ->
                        case evaluateExpression context listElement of
                            Err err ->
                                Err
                                    (DescribePathNode
                                        ("Failed to evaluate list item " ++ String.fromInt listElementIndex ++ ": ")
                                        err
                                    )

                            Ok ok ->
                                Ok ok
                    )
                    listElements
            of
                Err error ->
                    Err error

                Ok listItemsValues ->
                    Ok (ListValue listItemsValues)

        ParseAndEvalExpression parseAndEval ->
            case evaluateParseAndEval context parseAndEval of
                Err error ->
                    Err
                        (DescribePathNode
                            ("Failed parse and evaluate of '" ++ describeExpression 1 parseAndEval.expression ++ "'")
                            error
                        )

                Ok value ->
                    Ok value

        KernelApplicationExpression application ->
            case evaluateExpression context application.argument of
                Err error ->
                    Err
                        (DescribePathNode
                            ("Failed to evaluate argument for kernel function " ++ application.functionName ++ ": ")
                            error
                        )

                Ok argument ->
                    case parseKernelFunctionFromName application.functionName of
                        Err error ->
                            Err (DescribePathEnd error)

                        Ok kernelFunction ->
                            Ok (kernelFunction argument)

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
            case evaluateExpression context tagged of
                Err err ->
                    Err (DescribePathNode ("Failed to evaluate tagged expression '" ++ tag ++ "': ") err)

                Ok ok ->
                    Ok ok


valueFromContextExpansionWithName : ( String, Value ) -> Value
valueFromContextExpansionWithName ( declName, declValue ) =
    ListValue [ valueFromString declName, declValue ]


kernelFunctions : Dict.Dict String KernelFunction
kernelFunctions =
    Dict.fromList
        [ ( "equal"
          , \arg ->
                valueFromBool
                    (mapFromListValueOrBlobValue { fromList = list_all_same, fromBlob = list_all_same } arg)
          )
        , ( "negate"
          , kernelFunction_Negate
          )
        , ( "length"
          , \arg ->
                valueFromInt
                    (mapFromListValueOrBlobValue { fromList = List.length, fromBlob = List.length } arg)
          )
        , ( "skip"
          , kernelFunctionExpectingExactlyTwoArguments
                { mapArg0 = \countValue -> intFromValue countValue
                , mapArg1 = Ok
                , apply =
                    \count sequence ->
                        mapFromListValueOrBlobValue
                            { fromList = \list -> ListValue (List.drop count list)
                            , fromBlob = \bytes -> BlobValue (List.drop count bytes)
                            }
                            sequence
                }
          )
        , ( "take"
          , kernelFunctionExpectingExactlyTwoArguments
                { mapArg0 = \countValue -> intFromValue countValue
                , mapArg1 = Ok
                , apply =
                    \count sequence ->
                        mapFromListValueOrBlobValue
                            { fromList = \list -> ListValue (List.take count list)
                            , fromBlob = \bytes -> BlobValue (List.take count bytes)
                            }
                            sequence
                }
          )
        , ( "reverse"
          , mapFromListValueOrBlobValue
                { fromList = \list -> ListValue (List.reverse list)
                , fromBlob = \bytes -> BlobValue (List.reverse bytes)
                }
          )
        , ( "concat"
          , kernel_function_concat
          )
        , ( "list_head"
          , kernelFunctionExpectingList
                (\list ->
                    case list of
                        head :: _ ->
                            head

                        [] ->
                            listValue_Empty
                )
          )
        , ( "add_int"
          , kernelFunctionExpectingListOfBigIntAndProducingBigInt BigInt.add (BigInt.fromInt 0)
          )
        , ( "mul_int"
          , kernelFunctionExpectingListOfBigIntAndProducingBigInt BigInt.mul (BigInt.fromInt 1)
          )
        , ( "is_sorted_ascending_int"
          , kernel_function_is_sorted_ascending_int
          )
        ]


kernelFunctionsNames : List ( String, Value )
kernelFunctionsNames =
    List.map (\name -> ( name, valueFromString name ))
        (Dict.keys kernelFunctions)


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
                    listValue_Empty

        ListValue _ ->
            listValue_Empty


kernel_function_concat : Value -> Value
kernel_function_concat value =
    case value of
        ListValue list ->
            case list of
                [] ->
                    listValue_Empty

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
            listValue_Empty


list_all_same : List a -> Bool
list_all_same list =
    case list of
        [] ->
            True

        first :: rest ->
            List.all ((==) first) rest


kernel_function_is_sorted_ascending_int : Value -> Value
kernel_function_is_sorted_ascending_int value =
    case value of
        BlobValue bytes ->
            valueFromBool (List.sort bytes == bytes)

        ListValue list ->
            case list of
                [] ->
                    valueFromBool True

                first :: rest ->
                    case intFromValue first of
                        Err _ ->
                            listValue_Empty

                        Ok firstInt ->
                            is_sorted_ascending_int_recursive rest firstInt


is_sorted_ascending_int_recursive : List Value -> Int -> Value
is_sorted_ascending_int_recursive remaining previous =
    case remaining of
        [] ->
            trueValue

        next :: rest ->
            case intFromValue next of
                Err _ ->
                    listValue_Empty

                Ok nextInt ->
                    if nextInt < previous then
                        falseValue

                    else
                        is_sorted_ascending_int_recursive rest nextInt


mapFromListValueOrBlobValue : { fromList : List Value -> a, fromBlob : List Int -> a } -> Value -> a
mapFromListValueOrBlobValue { fromList, fromBlob } value =
    case value of
        ListValue list ->
            fromList list

        BlobValue blob ->
            fromBlob blob


evaluateParseAndEval : EvalContext -> ParseAndEvalExpressionStructure -> Result (PathDescription String) Value
evaluateParseAndEval context parseAndEval =
    case evaluateExpression context parseAndEval.environment of
        Err error ->
            Err
                (DescribePathNode
                    ("Failed to evaluate environment '" ++ describeExpression 1 parseAndEval.environment ++ "'")
                    error
                )

        Ok environmentValue ->
            case evaluateExpression context parseAndEval.expression of
                Err error ->
                    Err
                        (DescribePathNode
                            ("Failed to evaluate expression '" ++ describeExpression 1 parseAndEval.expression ++ "'")
                            error
                        )

                Ok functionValue ->
                    case parseExpressionFromValue functionValue of
                        Err error ->
                            Err
                                (DescribePathNode
                                    ("Failed to parse expression from value '" ++ describeValue 3 functionValue ++ "'")
                                    (DescribePathEnd error)
                                )

                        Ok functionExpression ->
                            evaluateExpression { environment = environmentValue } functionExpression


kernelFunctionExpectingListOfBigIntAndProducingBigInt :
    (BigInt.BigInt -> BigInt.BigInt -> BigInt.BigInt)
    -> BigInt.BigInt
    -> KernelFunction
kernelFunctionExpectingListOfBigIntAndProducingBigInt combine seed =
    let
        combineRecursive : List Value -> BigInt.BigInt -> Value
        combineRecursive remainingList aggregate =
            case remainingList of
                [] ->
                    valueFromBigInt aggregate

                itemValue :: following ->
                    case bigIntFromValue itemValue of
                        Err _ ->
                            listValue_Empty

                        Ok itemInt ->
                            combineRecursive following (combine aggregate itemInt)
    in
    kernelFunctionExpectingList
        (\list -> combineRecursive list seed)


kernelFunctionExpectingExactlyTwoArguments :
    { mapArg0 : Value -> Result String arg0
    , mapArg1 : Value -> Result String arg1
    , apply : arg0 -> arg1 -> Value
    }
    -> KernelFunction
kernelFunctionExpectingExactlyTwoArguments configuration =
    kernelFunctionExpectingList
        (\list ->
            case list of
                [ arg0Value, arg1Value ] ->
                    case configuration.mapArg0 arg0Value of
                        Err _ ->
                            listValue_Empty

                        Ok arg0 ->
                            case configuration.mapArg1 arg1Value of
                                Err _ ->
                                    listValue_Empty

                                Ok arg1 ->
                                    configuration.apply arg0 arg1

                _ ->
                    listValue_Empty
        )


kernelFunctionExpectingList : (List Value -> Value) -> KernelFunction
kernelFunctionExpectingList continueWithList value =
    case value of
        ListValue list ->
            continueWithList list

        _ ->
            listValue_Empty


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
                        String.join "," (List.map (describeExpression (depthLimit - 1)) list)
                   )
                ++ "]"

        LiteralExpression literal ->
            "literal(" ++ describeValue (depthLimit - 1) literal ++ ")"

        ParseAndEvalExpression parseAndEval ->
            "parse-and-eval("
                ++ (if depthLimit < 1 then
                        "..."

                    else
                        describeExpression (depthLimit - 1) parseAndEval.expression
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
    case string of
        "list_head" ->
            stringAsValue_list_head

        "skip" ->
            stringAsValue_skip

        "equal" ->
            stringAsValue_equal

        "EQ" ->
            stringAsValue_EQ

        "GT" ->
            stringAsValue_GT

        "LT" ->
            stringAsValue_LT

        "String" ->
            stringAsValue_String

        "Nothing" ->
            stringAsValue_Nothing

        "Just" ->
            stringAsValue_Just

        "KernelApplication" ->
            stringAsValue_KernelApplication

        "Literal" ->
            stringAsValue_Literal

        "List" ->
            stringAsValue_List

        "ParseAndEval" ->
            stringAsValue_ParseAndEval

        "Conditional" ->
            stringAsValue_Conditional

        "Environment" ->
            stringAsValue_Environment

        "Function" ->
            stringAsValue_Function

        "StringTag" ->
            stringAsValue_StringTag

        _ ->
            computeValueFromString string


computeValueFromString : String -> Value
computeValueFromString string =
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
    case continueRecursive values [] of
        Err err ->
            Err ("Failed to map list items to chars: " ++ err)

        Ok chars ->
            Ok (String.fromList (List.reverse chars))


valueFromBigInt : BigInt.BigInt -> Value
valueFromBigInt bigInt =
    BlobValue (blobValueFromBigInt bigInt)


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
                case String.toInt (BigInt.toString intValue) of
                    Nothing ->
                        Nothing

                    Just byte ->
                        Just [ byte ]

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
valueFromInt int =
    case int of
        1 ->
            valueFromInt_1

        2 ->
            valueFromInt_2

        3 ->
            valueFromInt_3

        4 ->
            valueFromInt_4

        _ ->
            computeValueFromInt int


computeValueFromInt : Int -> Value
computeValueFromInt int =
    BlobValue (blobValueFromInt int)


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
intFromValue value =
    case value of
        ListValue _ ->
            Err "Only a BlobValue can encode an integer."

        BlobValue blobBytes ->
            case blobBytes of
                [] ->
                    Err "Empty blob does not encode an integer."

                sign :: intValueBytes ->
                    case sign of
                        4 ->
                            intFromUnsignedBlobValue intValueBytes

                        2 ->
                            Result.map negate (intFromUnsignedBlobValue intValueBytes)

                        _ ->
                            Err ("Unexpected value for sign byte: " ++ String.fromInt sign)


intFromUnsignedBlobValue : List Int -> Result String Int
intFromUnsignedBlobValue intValueBytes =
    case intValueBytes of
        [] ->
            Err "Empty blob does not encode an integer."

        [ b1 ] ->
            Ok b1

        [ b1, b2 ] ->
            Ok ((b1 * 256) + b2)

        [ b1, b2, b3 ] ->
            Ok ((b1 * 65536) + (b2 * 256) + b3)

        [ b1, b2, b3, b4 ] ->
            Ok ((b1 * 16777216) + (b2 * 65536) + (b3 * 256) + b4)

        [ b1, b2, b3, b4, b5 ] ->
            Ok ((b1 * 4294967296) + (b2 * 16777216) + (b3 * 65536) + (b4 * 256) + b5)

        [ b1, b2, b3, b4, b5, b6 ] ->
            Ok ((b1 * 1099511627776) + (b2 * 4294967296) + (b3 * 16777216) + (b4 * 65536) + (b5 * 256) + b6)

        _ ->
            Err
                ("Failed to map to int - unsupported number of bytes: "
                    ++ String.fromInt (List.length intValueBytes)
                )


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
                    Ok (BigInt.negate (bigIntFromUnsignedBlobValue intValueBytes))

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
hexadecimalRepresentationFromBlobValue bytes =
    String.join ""
        (List.map (\byte -> String.padLeft 2 '0' (Hex.toString byte)) bytes)


encodeExpressionAsValue : Expression -> Value
encodeExpressionAsValue expression =
    case expression of
        LiteralExpression literal ->
            encodeUnionToPineValue
                stringAsValue_Literal
                literal

        ListExpression listExpr ->
            encodeUnionToPineValue
                stringAsValue_List
                (ListValue (List.map encodeExpressionAsValue listExpr))

        ParseAndEvalExpression parseAndEval ->
            encodeUnionToPineValue
                stringAsValue_ParseAndEval
                (ListValue
                    [ ListValue [ stringAsValue_environment, encodeExpressionAsValue parseAndEval.environment ]
                    , ListValue [ stringAsValue_expression, encodeExpressionAsValue parseAndEval.expression ]
                    ]
                )

        KernelApplicationExpression app ->
            encodeUnionToPineValue
                stringAsValue_KernelApplication
                (ListValue
                    [ ListValue [ stringAsValue_argument, encodeExpressionAsValue app.argument ]
                    , ListValue [ stringAsValue_functionName, valueFromString app.functionName ]
                    ]
                )

        ConditionalExpression conditional ->
            encodeUnionToPineValue
                stringAsValue_Conditional
                (ListValue
                    [ ListValue [ stringAsValue_condition, encodeExpressionAsValue conditional.condition ]
                    , ListValue [ stringAsValue_ifFalse, encodeExpressionAsValue conditional.ifFalse ]
                    , ListValue [ stringAsValue_ifTrue, encodeExpressionAsValue conditional.ifTrue ]
                    ]
                )

        EnvironmentExpression ->
            environmentExpressionAsValue

        StringTagExpression tag tagged ->
            encodeUnionToPineValue
                stringAsValue_StringTag
                (ListValue
                    [ valueFromString tag
                    , encodeExpressionAsValue tagged
                    ]
                )


environmentExpressionAsValue : Value
environmentExpressionAsValue =
    encodeUnionToPineValue
        stringAsValue_Environment
        listValue_Empty


parseExpressionFromValue : Value -> Result String Expression
parseExpressionFromValue =
    parseUnionFromPineValue parseExpressionFromValueDict


parseExpressionFromValueDict : List ( ( String, Value ), Value -> Result String Expression )
parseExpressionFromValueDict =
    List.map
        (\( tagName, parse ) ->
            ( ( tagName, valueFromString tagName ), parse )
        )
        [ ( "Literal"
          , \value -> Ok (LiteralExpression value)
          )
        , ( "List"
          , parseListExpression
          )
        , ( "ParseAndEval"
          , \value ->
                case parseParseAndEvalExpression value of
                    Ok parseAndEval ->
                        Ok (ParseAndEvalExpression parseAndEval)

                    Err err ->
                        Err err
          )
        , ( "KernelApplication"
          , \value ->
                case parseKernelApplicationExpression value of
                    Ok kernelApplication ->
                        Ok (KernelApplicationExpression kernelApplication)

                    Err err ->
                        Err err
          )
        , ( "Conditional"
          , \value ->
                case parseConditionalExpression value of
                    Ok conditional ->
                        Ok (ConditionalExpression conditional)

                    Err err ->
                        Err err
          )
        , ( "Environment"
          , \_ -> Ok environmentExpr
          )
        , ( "StringTag"
          , \value ->
                case parseListWithExactlyTwoElements value of
                    Err err ->
                        Err err

                    Ok ( tagValue, taggedValue ) ->
                        case stringFromValue tagValue of
                            Err err ->
                                Err err

                            Ok tag ->
                                case parseExpressionFromValue taggedValue of
                                    Err err ->
                                        Err err

                                    Ok tagged ->
                                        Ok (StringTagExpression tag tagged)
          )
        ]


parseListExpression : Value -> Result String Expression
parseListExpression value =
    let
        parseListRecursively : List Expression -> List Value -> Result String Expression
        parseListRecursively aggregate remaining =
            case remaining of
                [] ->
                    Ok (ListExpression (List.reverse aggregate))

                itemValue :: rest ->
                    case parseExpressionFromValue itemValue of
                        Err itemErr ->
                            Err
                                ("Failed to parse list item at index "
                                    ++ String.fromInt (List.length aggregate)
                                    ++ ": "
                                    ++ itemErr
                                )

                        Ok item ->
                            parseListRecursively (item :: aggregate) rest
    in
    case value of
        BlobValue _ ->
            Err "Is not list but blob"

        ListValue list ->
            parseListRecursively [] list


parseParseAndEvalExpression : Value -> Result String ParseAndEvalExpressionStructure
parseParseAndEvalExpression value =
    case value of
        BlobValue _ ->
            Err "Is not list but blob"

        ListValue list ->
            case parseListOfPairs list of
                Err err ->
                    Err ("Failed to parse kernel application expression: " ++ err)

                Ok pairs ->
                    case
                        Common.listFind
                            (\( fieldNameValue, _ ) ->
                                fieldNameValue == stringAsValue_expression
                            )
                            pairs
                    of
                        Nothing ->
                            Err "Did not find field 'expression'"

                        Just ( _, expressionValue ) ->
                            case parseExpressionFromValue expressionValue of
                                Err error ->
                                    Err ("Failed to parse field 'expression': " ++ error)

                                Ok expression ->
                                    case
                                        Common.listFind
                                            (\( fieldNameValue, _ ) ->
                                                fieldNameValue == stringAsValue_environment
                                            )
                                            pairs
                                    of
                                        Nothing ->
                                            Err "Did not find field 'environment'"

                                        Just ( _, environmentValue ) ->
                                            case parseExpressionFromValue environmentValue of
                                                Err error ->
                                                    Err ("Failed to parse field 'environment': " ++ error)

                                                Ok environment ->
                                                    Ok
                                                        { expression = expression
                                                        , environment = environment
                                                        }


parseKernelApplicationExpression : Value -> Result String KernelApplicationExpressionStructure
parseKernelApplicationExpression expressionValue =
    case expressionValue of
        BlobValue _ ->
            Err "Is not list but blob"

        ListValue list ->
            case parseListOfPairs list of
                Err err ->
                    Err ("Failed to parse kernel application expression: " ++ err)

                Ok pairs ->
                    case
                        Common.listFind
                            (\( fieldNameValue, _ ) ->
                                fieldNameValue == stringAsValue_functionName
                            )
                            pairs
                    of
                        Nothing ->
                            Err "Did not find field 'functionName'"

                        Just ( _, functionNameValue ) ->
                            case
                                Common.listFind
                                    (\( _, cvalue ) -> cvalue == functionNameValue)
                                    kernelFunctionsNames
                            of
                                Nothing ->
                                    Err
                                        ("Unexpected value for field 'functionName': "
                                            ++ Result.withDefault "not a string" (stringFromValue functionNameValue)
                                        )

                                Just ( functionName, _ ) ->
                                    case
                                        Common.listFind
                                            (\( fieldNameValue, _ ) ->
                                                fieldNameValue == stringAsValue_argument
                                            )
                                            pairs
                                    of
                                        Nothing ->
                                            Err "Did not find field 'argument'"

                                        Just ( _, argumentValue ) ->
                                            case parseExpressionFromValue argumentValue of
                                                Err error ->
                                                    Err ("Failed to parse field 'argument': " ++ error)

                                                Ok argument ->
                                                    Ok
                                                        { functionName = functionName
                                                        , argument = argument
                                                        }


parseKernelFunctionFromName : String -> Result String KernelFunction
parseKernelFunctionFromName functionName =
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


parseConditionalExpression : Value -> Result String ConditionalExpressionStructure
parseConditionalExpression expressionValue =
    case expressionValue of
        BlobValue _ ->
            Err "Is not list but blob"

        ListValue list ->
            case parseListOfPairs list of
                Err err ->
                    Err ("Failed to parse kernel application expression: " ++ err)

                Ok pairs ->
                    case
                        Common.listFind
                            (\( fieldNameValue, _ ) ->
                                fieldNameValue == stringAsValue_condition
                            )
                            pairs
                    of
                        Nothing ->
                            Err "Did not find field 'condition'"

                        Just ( _, conditionValue ) ->
                            case parseExpressionFromValue conditionValue of
                                Err error ->
                                    Err ("Failed to parse field 'condition': " ++ error)

                                Ok condition ->
                                    case
                                        Common.listFind
                                            (\( fieldNameValue, _ ) ->
                                                fieldNameValue == stringAsValue_ifTrue
                                            )
                                            pairs
                                    of
                                        Nothing ->
                                            Err "Did not find field 'ifTrue'"

                                        Just ( _, ifTrueValue ) ->
                                            case parseExpressionFromValue ifTrueValue of
                                                Err error ->
                                                    Err ("Failed to parse field 'ifTrue': " ++ error)

                                                Ok ifTrue ->
                                                    case
                                                        Common.listFind
                                                            (\( fieldNameValue, _ ) ->
                                                                fieldNameValue == stringAsValue_ifFalse
                                                            )
                                                            pairs
                                                    of
                                                        Nothing ->
                                                            Err "Did not find field 'ifFalse'"

                                                        Just ( _, ifFalseValue ) ->
                                                            case parseExpressionFromValue ifFalseValue of
                                                                Err error ->
                                                                    Err ("Failed to parse field 'ifFalse': " ++ error)

                                                                Ok ifFalse ->
                                                                    Ok
                                                                        { condition = condition
                                                                        , ifTrue = ifTrue
                                                                        , ifFalse = ifFalse
                                                                        }


encodeUnionToPineValue : Value -> Value -> Value
encodeUnionToPineValue tagNameValue unionTagValue =
    ListValue [ tagNameValue, unionTagValue ]


parseUnionFromPineValue : List ( ( String, Value ), Value -> Result String a ) -> Value -> Result String a
parseUnionFromPineValue tags value =
    case parseListWithExactlyTwoElements value of
        Err err ->
            Err ("Failed to parse union: " ++ err)

        Ok ( tagNameValue, unionTagValue ) ->
            case
                Common.listFind
                    (\( ( _, availableTagValue ), _ ) -> availableTagValue == tagNameValue)
                    tags
            of
                Nothing ->
                    Err
                        (case stringFromValue tagNameValue of
                            Ok tagName ->
                                "Unexpected tag name: " ++ tagName

                            Err err ->
                                "Failed decoding tag name: " ++ err
                        )

                Just ( ( tagName, _ ), tagDecode ) ->
                    case tagDecode unionTagValue of
                        Err err ->
                            Err (("Failed to parse value for tag " ++ tagName ++ ": ") ++ err)

                        Ok ok ->
                            Ok ok


parseListOfPairs : List Value -> Result String (List ( Value, Value ))
parseListOfPairs list =
    let
        continueRecursive : List Value -> List ( Value, Value ) -> Result String (List ( Value, Value ))
        continueRecursive remaining aggregate =
            case remaining of
                [] ->
                    Ok (List.reverse aggregate)

                itemValue :: rest ->
                    case itemValue of
                        BlobValue _ ->
                            Err "Is not list but blob"

                        ListValue [ first, second ] ->
                            continueRecursive rest (( first, second ) :: aggregate)

                        ListValue innerList ->
                            Err
                                ("Unexpected number of list items for pair: "
                                    ++ String.fromInt (List.length innerList)
                                )
    in
    continueRecursive list []


parseListWithExactlyTwoElements : Value -> Result String ( Value, Value )
parseListWithExactlyTwoElements value =
    case value of
        BlobValue _ ->
            Err "Is not list but blob"

        ListValue list ->
            case list of
                [ a, b ] ->
                    Ok ( a, b )

                _ ->
                    Err ("Unexpected number of elements in list: Not 2 but " ++ String.fromInt (List.length list))


environmentExpr : Expression
environmentExpr =
    EnvironmentExpression


stringAsValue_list_head : Value
stringAsValue_list_head =
    computeValueFromString "list_head"


stringAsValue_skip : Value
stringAsValue_skip =
    computeValueFromString "skip"


stringAsValue_equal : Value
stringAsValue_equal =
    computeValueFromString "equal"


stringAsValue_Literal : Value
stringAsValue_Literal =
    computeValueFromString "Literal"


stringAsValue_List : Value
stringAsValue_List =
    computeValueFromString "List"


stringAsValue_ParseAndEval : Value
stringAsValue_ParseAndEval =
    computeValueFromString "ParseAndEval"


stringAsValue_KernelApplication : Value
stringAsValue_KernelApplication =
    computeValueFromString "KernelApplication"


stringAsValue_Conditional : Value
stringAsValue_Conditional =
    computeValueFromString "Conditional"


stringAsValue_Environment : Value
stringAsValue_Environment =
    computeValueFromString "Environment"


stringAsValue_Function : Value
stringAsValue_Function =
    computeValueFromString "Function"


stringAsValue_StringTag : Value
stringAsValue_StringTag =
    computeValueFromString "StringTag"


stringAsValue_functionName : Value
stringAsValue_functionName =
    computeValueFromString "functionName"


stringAsValue_argument : Value
stringAsValue_argument =
    computeValueFromString "argument"


stringAsValue_condition : Value
stringAsValue_condition =
    computeValueFromString "condition"


stringAsValue_ifTrue : Value
stringAsValue_ifTrue =
    computeValueFromString "ifTrue"


stringAsValue_ifFalse : Value
stringAsValue_ifFalse =
    computeValueFromString "ifFalse"


stringAsValue_environment : Value
stringAsValue_environment =
    computeValueFromString "environment"


stringAsValue_expression : Value
stringAsValue_expression =
    computeValueFromString "expression"


stringAsValue_EQ : Value
stringAsValue_EQ =
    computeValueFromString "EQ"


stringAsValue_LT : Value
stringAsValue_LT =
    computeValueFromString "LT"


stringAsValue_GT : Value
stringAsValue_GT =
    computeValueFromString "GT"


stringAsValue_String : Value
stringAsValue_String =
    computeValueFromString "String"


stringAsValue_Just : Value
stringAsValue_Just =
    computeValueFromString "Just"


stringAsValue_Nothing : Value
stringAsValue_Nothing =
    computeValueFromString "Nothing"


valueFromInt_1 : Value
valueFromInt_1 =
    computeValueFromInt 1


valueFromInt_2 : Value
valueFromInt_2 =
    computeValueFromInt 2


valueFromInt_3 : Value
valueFromInt_3 =
    computeValueFromInt 3


valueFromInt_4 : Value
valueFromInt_4 =
    computeValueFromInt 4


listValue_Empty : Value
listValue_Empty =
    ListValue []
