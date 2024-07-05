module Pine exposing
    ( ConditionalExpressionStructure
    , EvalContext
    , Expression(..)
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
    , kernelFunction_negate
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
    | KernelApplicationExpression Expression String
    | ConditionalExpression ConditionalExpressionStructure
    | EnvironmentExpression
    | StringTagExpression String Expression


type alias ParseAndEvalExpressionStructure =
    { expression : Expression
    , environment : Expression
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
            evaluateListExpression [] context listElements

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

        KernelApplicationExpression argumentExpr functionName ->
            case evaluateExpression context argumentExpr of
                Err error ->
                    Err
                        (DescribePathNode
                            ("Failed to evaluate argument for kernel function " ++ functionName ++ ": ")
                            error
                        )

                Ok argument ->
                    case parseKernelFunctionFromName functionName of
                        Err error ->
                            Err (DescribePathEnd error)

                        Ok kernelFunction ->
                            Ok (kernelFunction argument)

        ConditionalExpression conditional ->
            case evaluateExpression context conditional.condition of
                Err error ->
                    Err (DescribePathNode "Failed to evaluate condition" error)

                Ok conditionValue ->
                    case conditionValue of
                        BlobValue [ 4 ] ->
                            evaluateExpression context conditional.ifTrue

                        BlobValue [ 2 ] ->
                            evaluateExpression context conditional.ifFalse

                        _ ->
                            Ok listValue_Empty

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
                    Err
                        (DescribePathNode
                            ("Failed to evaluate tagged expression '" ++ tag ++ "': ")
                            err
                        )

                Ok ok ->
                    Ok ok


evaluateListExpression : List Value -> EvalContext -> List Expression -> Result (PathDescription String) Value
evaluateListExpression completedItems context remainingItems =
    case remainingItems of
        [] ->
            Ok (ListValue (List.reverse completedItems))

        currentItem :: followingItems ->
            case evaluateExpression context currentItem of
                Err err ->
                    Err
                        (DescribePathNode
                            ("Failed to evaluate list item " ++ String.fromInt (List.length completedItems) ++ ": ")
                            err
                        )

                Ok itemValue ->
                    evaluateListExpression
                        (itemValue :: completedItems)
                        context
                        followingItems


valueFromContextExpansionWithName : ( String, Value ) -> Value
valueFromContextExpansionWithName ( declName, declValue ) =
    ListValue [ valueFromString declName, declValue ]


kernelFunctions : Dict.Dict String KernelFunction
kernelFunctions =
    Dict.fromList
        [ ( "equal"
          , kernelFunction_equal
          )
        , ( "negate"
          , kernelFunction_negate
          )
        , ( "length"
          , kernelFunction_length
          )
        , ( "skip"
          , kernelFunction_skip
          )
        , ( "take"
          , kernelFunction_take
          )
        , ( "reverse"
          , kernelFunction_reverse
          )
        , ( "concat"
          , kernelFunction_concat
          )
        , ( "list_head"
          , kernelFunction_list_head
          )
        , ( "add_int"
          , kernelFunction_add_int
          )
        , ( "mul_int"
          , kernelFunction_mul_int
          )
        , ( "is_sorted_ascending_int"
          , kernelFunction_is_sorted_ascending_int
          )
        ]


kernelFunctionsNames : List ( Value, String )
kernelFunctionsNames =
    Dict.foldl
        (\name _ aggregate -> ( valueFromString name, name ) :: aggregate)
        []
        kernelFunctions


kernelFunction_equal : KernelFunction
kernelFunction_equal arg =
    valueFromBool
        (case arg of
            ListValue list ->
                case list of
                    [] ->
                        True

                    first :: rest ->
                        list_all_same first rest

            BlobValue bytes ->
                case bytes of
                    [] ->
                        True

                    first :: rest ->
                        list_all_same first rest
        )


kernelFunction_negate : KernelFunction
kernelFunction_negate value =
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


kernelFunction_length : KernelFunction
kernelFunction_length arg =
    valueFromInt
        (case arg of
            ListValue list ->
                List.length list

            BlobValue bytes ->
                List.length bytes
        )


kernelFunction_skip : KernelFunction
kernelFunction_skip arg =
    case arg of
        ListValue [ skipValue, sequence ] ->
            case intFromValue skipValue of
                Ok count ->
                    case sequence of
                        ListValue list ->
                            ListValue (List.drop count list)

                        BlobValue bytes ->
                            BlobValue (List.drop count bytes)

                Err _ ->
                    listValue_Empty

        _ ->
            listValue_Empty


kernelFunction_take : KernelFunction
kernelFunction_take arg =
    case arg of
        ListValue [ skipValue, sequence ] ->
            case intFromValue skipValue of
                Ok count ->
                    case sequence of
                        ListValue list ->
                            ListValue (List.take count list)

                        BlobValue bytes ->
                            BlobValue (List.take count bytes)

                Err _ ->
                    listValue_Empty

        _ ->
            listValue_Empty


kernelFunction_reverse : KernelFunction
kernelFunction_reverse arg =
    case arg of
        ListValue list ->
            ListValue (List.reverse list)

        BlobValue bytes ->
            BlobValue (List.reverse bytes)


kernelFunction_concat : Value -> Value
kernelFunction_concat value =
    case value of
        ListValue list ->
            case list of
                [] ->
                    listValue_Empty

                (BlobValue _) :: _ ->
                    kernelFunction_concat_blob [] list

                (ListValue _) :: _ ->
                    kernelFunction_concat_list [] list

        _ ->
            listValue_Empty


kernelFunction_concat_blob : List Int -> List Value -> Value
kernelFunction_concat_blob bytes remainingItems =
    case remainingItems of
        (BlobValue currentBytes) :: followingItems ->
            kernelFunction_concat_blob (List.concat [ bytes, currentBytes ]) followingItems

        (ListValue _) :: followingItems ->
            kernelFunction_concat_blob bytes followingItems

        [] ->
            BlobValue bytes


kernelFunction_concat_list : List Value -> List Value -> Value
kernelFunction_concat_list flattenedItems remainingItems =
    case remainingItems of
        (ListValue currentItems) :: followingItems ->
            kernelFunction_concat_list (List.concat [ flattenedItems, currentItems ]) followingItems

        (BlobValue _) :: followingItems ->
            kernelFunction_concat_list flattenedItems followingItems

        [] ->
            ListValue flattenedItems


kernelFunction_list_head : Value -> Value
kernelFunction_list_head value =
    case value of
        ListValue (head :: _) ->
            head

        _ ->
            listValue_Empty


kernelFunction_add_int : Value -> Value
kernelFunction_add_int value =
    case value of
        ListValue list ->
            kernelFunction_add_int_list (BigInt.fromInt 0) list

        _ ->
            listValue_Empty


kernelFunction_add_int_list : BigInt.BigInt -> List Value -> Value
kernelFunction_add_int_list aggregate list =
    case list of
        [] ->
            valueFromBigInt aggregate

        nextValue :: rest ->
            case bigIntFromValue nextValue of
                Ok nextInt ->
                    kernelFunction_add_int_list (BigInt.add aggregate nextInt) rest

                Err _ ->
                    listValue_Empty


kernelFunction_mul_int : Value -> Value
kernelFunction_mul_int value =
    case value of
        ListValue list ->
            kernelFunction_mul_int_list (BigInt.fromInt 1) list

        _ ->
            listValue_Empty


kernelFunction_mul_int_list : BigInt.BigInt -> List Value -> Value
kernelFunction_mul_int_list aggregate list =
    case list of
        [] ->
            valueFromBigInt aggregate

        nextValue :: rest ->
            case bigIntFromValue nextValue of
                Ok nextInt ->
                    kernelFunction_mul_int_list (BigInt.mul aggregate nextInt) rest

                Err _ ->
                    listValue_Empty


kernelFunction_is_sorted_ascending_int : Value -> Value
kernelFunction_is_sorted_ascending_int value =
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


list_all_same : a -> List a -> Bool
list_all_same item list =
    case list of
        [] ->
            True

        first :: rest ->
            if first == item then
                list_all_same item rest

            else
                False


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


evaluateParseAndEval : EvalContext -> ParseAndEvalExpressionStructure -> Result (PathDescription String) Value
evaluateParseAndEval context parseAndEval =
    case evaluateExpression context parseAndEval.environment of
        Err error ->
            Err
                (DescribePathNode
                    ("Failed to evaluate environment '"
                        ++ describeExpression 1 parseAndEval.environment
                        ++ "'"
                    )
                    error
                )

        Ok environmentValue ->
            case evaluateExpression context parseAndEval.expression of
                Err error ->
                    Err
                        (DescribePathNode
                            ("Failed to evaluate expression '"
                                ++ describeExpression 1 parseAndEval.expression
                                ++ "'"
                            )
                            error
                        )

                Ok functionValue ->
                    case parseExpressionFromValue functionValue of
                        Err error ->
                            Err
                                (DescribePathNode
                                    ("Failed to parse expression from value '"
                                        ++ describeValue 3 functionValue
                                        ++ "'"
                                    )
                                    (DescribePathEnd error)
                                )

                        Ok functionExpression ->
                            evaluateExpression { environment = environmentValue } functionExpression


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
                            ++ "]"
                   )

        LiteralExpression literal ->
            "literal("
                ++ describeValue (depthLimit - 1) literal
                ++ ")"

        ParseAndEvalExpression parseAndEval ->
            "parse-and-eval("
                ++ (if depthLimit < 1 then
                        "..."

                    else
                        describeExpression (depthLimit - 1) parseAndEval.expression
                            ++ ")"
                   )

        KernelApplicationExpression _ functionName ->
            "kernel-application("
                ++ functionName
                ++ ")"

        ConditionalExpression _ ->
            "conditional"

        EnvironmentExpression ->
            "environment"

        StringTagExpression tag tagged ->
            "string-tag-"
                ++ tag
                ++ "("
                ++ describeExpression (depthLimit - 1) tagged
                ++ ")"


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
                                    ++ "]"
                           )
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
    String.join "\n"
        (List.map (\line -> "" ++ prefix ++ line)
            (String.lines text)
        )


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
    if value == stringAsValue_Literal then
        Ok "Literal"

    else if value == stringAsValue_List then
        Ok "List"

    else if value == stringAsValue_ParseAndEval then
        Ok "ParseAndEval"

    else if value == stringAsValue_KernelApplication then
        Ok "KernelApplication"

    else if value == stringAsValue_Conditional then
        Ok "Conditional"

    else if value == stringAsValue_Environment then
        Ok "Environment"

    else if value == stringAsValue_Function then
        Ok "Function"

    else if value == stringAsValue_StringTag then
        Ok "StringTag"

    else
        case value of
            ListValue charsValues ->
                stringFromListValue charsValues

            _ ->
                Err "Not a list"


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
    case unsignedBytesFromIntValue value of
        Nothing ->
            [ signByte ]

        Just unsignedBytes ->
            signByte :: unsignedBytes


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
                sign :: firstUnsigned :: followingUnsigned ->
                    case sign of
                        4 ->
                            Ok (intFromUnsignedBlobValue followingUnsigned firstUnsigned)

                        2 ->
                            Ok -(intFromUnsignedBlobValue followingUnsigned firstUnsigned)

                        _ ->
                            Err ("Unexpected value for sign byte: " ++ String.fromInt sign)

                _ ->
                    Err "Blob needs at least two bytes to encode an integer."


intFromUnsignedBlobValue : List Int -> Int -> Int
intFromUnsignedBlobValue intValueBytes upper =
    case intValueBytes of
        [] ->
            upper

        b :: rest ->
            intFromUnsignedBlobValue rest (upper * 256 + b)


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
    bigIntFromUnsignedBlobValueContinue
        (BigInt.fromInt 0)
        intValueBytes


bigIntFromUnsignedBlobValueContinue : BigInt.BigInt -> List Int -> BigInt.BigInt
bigIntFromUnsignedBlobValueContinue aggregate intValueBytes =
    case intValueBytes of
        nextByte :: followingBytes ->
            bigIntFromUnsignedBlobValueContinue
                (BigInt.add (BigInt.fromInt nextByte) (BigInt.mul (BigInt.fromInt 0x0100) aggregate))
                followingBytes

        [] ->
            aggregate


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
            encodeListExpressionAsValueReversed [] (List.reverse listExpr)

        ParseAndEvalExpression parseAndEval ->
            encodeUnionToPineValue
                stringAsValue_ParseAndEval
                (ListValue
                    [ ListValue [ stringAsValue_environment, encodeExpressionAsValue parseAndEval.environment ]
                    , ListValue [ stringAsValue_expression, encodeExpressionAsValue parseAndEval.expression ]
                    ]
                )

        KernelApplicationExpression argument functionName ->
            encodeUnionToPineValue
                stringAsValue_KernelApplication
                (ListValue
                    [ ListValue [ stringAsValue_argument, encodeExpressionAsValue argument ]
                    , ListValue [ stringAsValue_functionName, valueFromString functionName ]
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


encodeListExpressionAsValueReversed : List Value -> List Expression -> Value
encodeListExpressionAsValueReversed encodedItems list =
    case list of
        [] ->
            encodeUnionToPineValue
                stringAsValue_List
                (ListValue encodedItems)

        itemExpr :: remaining ->
            encodeListExpressionAsValueReversed
                (encodeExpressionAsValue itemExpr :: encodedItems)
                remaining


environmentExpressionAsValue : Value
environmentExpressionAsValue =
    encodeUnionToPineValue
        stringAsValue_Environment
        listValue_Empty


parseExpressionFromValue : Value -> Result String Expression
parseExpressionFromValue exprValue =
    case parseListWithExactlyTwoElements exprValue of
        Err err ->
            Err ("Failed to parse union: " ++ err)

        Ok ( tagNameValue, unionTagValue ) ->
            case stringFromValue tagNameValue of
                Err err ->
                    Err ("Failed parsing tag name: " ++ err)

                Ok tagName ->
                    case tagName of
                        "Literal" ->
                            Ok (LiteralExpression unionTagValue)

                        "List" ->
                            parseListExpression unionTagValue

                        "ParseAndEval" ->
                            case parseParseAndEvalExpression unionTagValue of
                                Ok parseAndEval ->
                                    Ok (ParseAndEvalExpression parseAndEval)

                                Err err ->
                                    Err err

                        "KernelApplication" ->
                            case parseKernelApplicationExpression unionTagValue of
                                Ok ( argument, functionName ) ->
                                    Ok (KernelApplicationExpression argument functionName)

                                Err err ->
                                    Err err

                        "Conditional" ->
                            case parseConditionalExpression unionTagValue of
                                Ok conditional ->
                                    Ok (ConditionalExpression conditional)

                                Err err ->
                                    Err err

                        "Environment" ->
                            Ok environmentExpr

                        "StringTag" ->
                            case parseListWithExactlyTwoElements unionTagValue of
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

                        _ ->
                            Err ("Unexpected expr tag: " ++ tagName)


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
                    case Common.assocListGet stringAsValue_expression pairs of
                        Nothing ->
                            Err "Did not find field 'expression'"

                        Just expressionValue ->
                            case parseExpressionFromValue expressionValue of
                                Err error ->
                                    Err ("Failed to parse field 'expression': " ++ error)

                                Ok expression ->
                                    case Common.assocListGet stringAsValue_environment pairs of
                                        Nothing ->
                                            Err "Did not find field 'environment'"

                                        Just environmentValue ->
                                            case parseExpressionFromValue environmentValue of
                                                Err error ->
                                                    Err ("Failed to parse field 'environment': " ++ error)

                                                Ok environment ->
                                                    Ok
                                                        { expression = expression
                                                        , environment = environment
                                                        }


parseKernelApplicationExpression : Value -> Result String ( Expression, String )
parseKernelApplicationExpression expressionValue =
    case expressionValue of
        BlobValue _ ->
            Err "Is not list but blob"

        ListValue list ->
            case parseListOfPairs list of
                Err err ->
                    Err ("Failed to parse kernel application expression: " ++ err)

                Ok pairs ->
                    case Common.assocListGet stringAsValue_functionName pairs of
                        Nothing ->
                            Err "Did not find field 'functionName'"

                        Just functionNameValue ->
                            case Common.assocListGet functionNameValue kernelFunctionsNames of
                                Nothing ->
                                    Err "Unexpected 'functionName'"

                                Just functionName ->
                                    case Common.assocListGet stringAsValue_argument pairs of
                                        Nothing ->
                                            Err "Did not find field 'argument'"

                                        Just argumentValue ->
                                            case parseExpressionFromValue argumentValue of
                                                Err error ->
                                                    Err ("Failed to parse field 'argument': " ++ error)

                                                Ok argument ->
                                                    Ok ( argument, functionName )


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
                    case Common.assocListGet stringAsValue_condition pairs of
                        Nothing ->
                            Err "Did not find field 'condition'"

                        Just conditionValue ->
                            case parseExpressionFromValue conditionValue of
                                Err error ->
                                    Err ("Failed to parse field 'condition': " ++ error)

                                Ok condition ->
                                    case Common.assocListGet stringAsValue_ifTrue pairs of
                                        Nothing ->
                                            Err "Did not find field 'ifTrue'"

                                        Just ifTrueValue ->
                                            case parseExpressionFromValue ifTrueValue of
                                                Err error ->
                                                    Err ("Failed to parse field 'ifTrue': " ++ error)

                                                Ok ifTrue ->
                                                    case Common.assocListGet stringAsValue_ifFalse pairs of
                                                        Nothing ->
                                                            Err "Did not find field 'ifFalse'"

                                                        Just ifFalseValue ->
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
                    Err
                        ("Unexpected number of elements in list: Not 2 but "
                            ++ String.fromInt (List.length list)
                        )


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
