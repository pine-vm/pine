module Pine exposing
    ( EvalEnvironment(..)
    , Expression(..)
    , KernelFunction
    , PathDescription(..)
    , Value(..)
    , applyKernelFunction
    , bigIntFromBlobValue
    , bigIntFromUnsignedBlobValue
    , bigIntFromValue
    , blobBytesFromChar
    , blobValueFromBigInt
    , computeValueFromString
    , countValueContent
    , displayStringFromPineError
    , emptyEvalEnvironment
    , encodeExpressionAsValue
    , environmentExpr
    , evalEnvironmentFromList
    , evaluateExpression
    , falseValue
    , intFromUnsignedBlobValue
    , intFromValue
    , kernelFunction_negate
    , listValue_Empty
    , parseExpressionFromValue
    , stringAsValue_Function
    , stringAsValue_List
    , stringAsValue_Literal
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
import Bitwise
import Hex


type Expression
    = LiteralExpression Value
    | ListExpression (List Expression)
    | ParseAndEvalExpression
        -- Encoded
        Expression
        -- Environment
        Expression
    | KernelApplicationExpression String Expression
    | ConditionalExpression
        -- Condition
        Expression
        -- False Branch
        Expression
        -- True Branch
        Expression
    | EnvironmentExpression
    | StringTagExpression String Expression


type Value
    = BlobValue (List Int)
    | ListValue (List Value)


type alias KernelFunction =
    Value -> Value


type EvalEnvironment
    = EvalEnvironment Value


type PathDescription a
    = DescribePathNode a (PathDescription a)
    | DescribePathEnd a


evalEnvironmentFromList : List Value -> EvalEnvironment
evalEnvironmentFromList list =
    EvalEnvironment (ListValue list)


emptyEvalEnvironment : EvalEnvironment
emptyEvalEnvironment =
    EvalEnvironment listValue_Empty


evaluateExpression : EvalEnvironment -> Expression -> Result (PathDescription String) Value
evaluateExpression context expression =
    case expression of
        LiteralExpression value ->
            Ok value

        ListExpression listElements ->
            evaluateListExpression [] context listElements

        ParseAndEvalExpression encodedExpr envExpr ->
            case evaluateParseAndEval context ( encodedExpr, envExpr ) of
                Err error ->
                    Err
                        (DescribePathNode
                            ("Failed parse and evaluate of '" ++ describeExpression 1 encodedExpr ++ "'")
                            error
                        )

                Ok value ->
                    Ok value

        KernelApplicationExpression functionName inputExpr ->
            evaluateExpressionKernelApplication
                context
                functionName
                inputExpr

        ConditionalExpression condition falseBranch trueBranch ->
            case evaluateExpression context condition of
                Err error ->
                    Err (DescribePathNode "Failed to evaluate condition" error)

                Ok conditionValue ->
                    case conditionValue of
                        BlobValue [ 4 ] ->
                            evaluateExpression context trueBranch

                        _ ->
                            evaluateExpression context falseBranch

        EnvironmentExpression ->
            let
                (EvalEnvironment environmentValue) =
                    context
            in
            Ok environmentValue

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


evaluateListExpression : List Value -> EvalEnvironment -> List Expression -> Result (PathDescription String) Value
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


evaluateExpressionKernelApplication : EvalEnvironment -> String -> Expression -> Result (PathDescription String) Value
evaluateExpressionKernelApplication context functionName inputExpr =
    case evaluateExpression context inputExpr of
        Err error ->
            Err
                (DescribePathNode
                    ("Failed to evaluate argument for kernel function " ++ functionName ++ ": ")
                    error
                )

        Ok input ->
            applyKernelFunction
                functionName
                input


applyKernelFunction : String -> Value -> Result (PathDescription String) Value
applyKernelFunction functionName input =
    case functionName of
        "equal" ->
            Ok (kernelFunction_equal input)

        "negate" ->
            Ok (kernelFunction_negate input)

        "length" ->
            Ok (kernelFunction_length input)

        "skip" ->
            Ok (kernelFunction_skip input)

        "take" ->
            Ok (kernelFunction_take input)

        "reverse" ->
            Ok (kernelFunction_reverse input)

        "concat" ->
            Ok (kernelFunction_concat input)

        "head" ->
            Ok (kernelFunction_head input)

        "int_add" ->
            Ok (kernelFunction_int_add input)

        "int_mul" ->
            Ok (kernelFunction_int_mul input)

        "int_is_sorted_asc" ->
            Ok (kernelFunction_int_is_sorted_asc input)

        "bit_and" ->
            Ok (kernelFunction_bit_and input)

        "bit_or" ->
            Ok (kernelFunction_bit_or input)

        "bit_xor" ->
            Ok (kernelFunction_bit_xor input)

        "bit_not" ->
            Ok (kernelFunction_bit_not input)

        "bit_shift_left" ->
            Ok (kernelFunction_bit_shift_left input)

        "bit_shift_right" ->
            Ok (kernelFunction_bit_shift_right input)

        _ ->
            Err
                (DescribePathEnd
                    ("Unknown kernel function: "
                        ++ functionName
                    )
                )


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
            kernelFunction_concat_list list

        _ ->
            listValue_Empty


kernelFunction_concat_list : List Value -> Value
kernelFunction_concat_list list =
    case list of
        firstItem :: remainingItems ->
            case firstItem of
                ListValue firstItems ->
                    if List.length firstItems == 0 then
                        kernelFunction_concat_list remainingItems

                    else
                        kernelFunction_concat_list_items
                            firstItems
                            remainingItems

                BlobValue firstBytes ->
                    kernelFunction_concat_blob_bytes
                        firstBytes
                        remainingItems

        _ ->
            listValue_Empty


kernelFunction_concat_blob_bytes : List Int -> List Value -> Value
kernelFunction_concat_blob_bytes bytes remainingItems =
    case remainingItems of
        nextItem :: followingItems ->
            case nextItem of
                BlobValue currentBytes ->
                    kernelFunction_concat_blob_bytes
                        (List.concat [ bytes, currentBytes ])
                        followingItems

                ListValue items ->
                    if List.length items == 0 then
                        kernelFunction_concat_blob_bytes bytes followingItems

                    else
                        listValue_Empty

        _ ->
            BlobValue bytes


kernelFunction_concat_list_items : List Value -> List Value -> Value
kernelFunction_concat_list_items flattenedItems remainingItems =
    case remainingItems of
        nextItem :: followingItems ->
            case nextItem of
                ListValue currentItems ->
                    kernelFunction_concat_list_items
                        (List.concat [ flattenedItems, currentItems ])
                        followingItems

                BlobValue _ ->
                    listValue_Empty

        _ ->
            ListValue flattenedItems


kernelFunction_head : Value -> Value
kernelFunction_head value =
    case value of
        ListValue (head :: _) ->
            head

        ListValue [] ->
            listValue_Empty

        BlobValue bytes ->
            BlobValue (List.take 1 bytes)


kernelFunction_int_add : Value -> Value
kernelFunction_int_add value =
    case value of
        ListValue list ->
            kernelFunction_int_add_list (BigInt.fromInt 0) list

        _ ->
            listValue_Empty


kernelFunction_int_add_list : BigInt.BigInt -> List Value -> Value
kernelFunction_int_add_list aggregate list =
    case list of
        nextValue :: rest ->
            case bigIntFromValue nextValue of
                Ok nextInt ->
                    kernelFunction_int_add_list (BigInt.add aggregate nextInt) rest

                Err _ ->
                    listValue_Empty

        _ ->
            valueFromBigInt aggregate


kernelFunction_int_mul : Value -> Value
kernelFunction_int_mul value =
    case value of
        ListValue list ->
            kernelFunction_int_mul_list (BigInt.fromInt 1) list

        _ ->
            listValue_Empty


kernelFunction_int_mul_list : BigInt.BigInt -> List Value -> Value
kernelFunction_int_mul_list aggregate list =
    case list of
        nextValue :: rest ->
            case bigIntFromValue nextValue of
                Ok nextInt ->
                    kernelFunction_int_mul_list (BigInt.mul aggregate nextInt) rest

                Err _ ->
                    listValue_Empty

        _ ->
            valueFromBigInt aggregate


kernelFunction_int_is_sorted_asc : Value -> Value
kernelFunction_int_is_sorted_asc value =
    case value of
        ListValue list ->
            case list of
                [] ->
                    valueFromBool True

                first :: rest ->
                    case intFromValue first of
                        Err _ ->
                            listValue_Empty

                        Ok firstInt ->
                            int_is_sorted_asc_recursive rest firstInt

        _ ->
            listValue_Empty


kernelFunction_bit_and : Value -> Value
kernelFunction_bit_and value =
    case value of
        ListValue ((BlobValue first) :: rest) ->
            kernelFunction_bit_and_recursive first rest

        _ ->
            listValue_Empty


kernelFunction_bit_or : Value -> Value
kernelFunction_bit_or value =
    case value of
        ListValue ((BlobValue first) :: rest) ->
            kernelFunction_bit_or_recursive first rest

        _ ->
            listValue_Empty


kernelFunction_bit_and_recursive : List Int -> List Value -> Value
kernelFunction_bit_and_recursive blob arguments =
    case arguments of
        next :: rest ->
            case next of
                BlobValue nextBlob ->
                    kernelFunction_bit_and_recursive
                        (bit_and_tuple []
                            (List.reverse blob)
                            (List.reverse nextBlob)
                        )
                        rest

                _ ->
                    listValue_Empty

        _ ->
            BlobValue blob


kernelFunction_bit_or_recursive : List Int -> List Value -> Value
kernelFunction_bit_or_recursive blob arguments =
    case arguments of
        next :: rest ->
            case next of
                BlobValue nextBlob ->
                    kernelFunction_bit_or_recursive
                        (bit_or_tuple
                            []
                            (List.reverse blob)
                            (List.reverse nextBlob)
                        )
                        rest

                _ ->
                    listValue_Empty

        _ ->
            BlobValue blob


kernelFunction_bit_xor : Value -> Value
kernelFunction_bit_xor value =
    case value of
        ListValue ((BlobValue first) :: rest) ->
            kernelFunction_bit_xor_recursive first rest

        _ ->
            listValue_Empty


kernelFunction_bit_xor_recursive : List Int -> List Value -> Value
kernelFunction_bit_xor_recursive blob arguments =
    case arguments of
        next :: rest ->
            case next of
                BlobValue nextBlob ->
                    kernelFunction_bit_xor_recursive
                        (bit_xor_tuple
                            []
                            (List.reverse blob)
                            (List.reverse nextBlob)
                        )
                        rest

                _ ->
                    listValue_Empty

        _ ->
            BlobValue blob


kernelFunction_bit_not : Value -> Value
kernelFunction_bit_not value =
    case value of
        BlobValue bytes ->
            BlobValue
                (kernelFunction_bit_not_bytes [] bytes)

        _ ->
            listValue_Empty


kernelFunction_bit_not_bytes : List Int -> List Int -> List Int
kernelFunction_bit_not_bytes reversedBytes remainingBytes =
    case remainingBytes of
        byte :: rest ->
            kernelFunction_bit_not_bytes
                ((255 - byte) :: reversedBytes)
                rest

        _ ->
            List.reverse reversedBytes


kernelFunction_bit_shift_left : Value -> Value
kernelFunction_bit_shift_left value =
    case value of
        ListValue [ shiftValue, BlobValue bytes ] ->
            case intFromValue shiftValue of
                Ok shiftInt ->
                    let
                        offsetBytes =
                            shiftInt // 8

                        offsetBits =
                            shiftInt - (offsetBytes * 8)

                        beforeOffsetBytes =
                            kernelFunction_bit_shift_left_recursive
                                []
                                (List.concat
                                    [ List.reverse (List.drop offsetBytes bytes)
                                    , List.repeat offsetBytes 0
                                    ]
                                )
                                0
                                offsetBits
                    in
                    BlobValue
                        (List.concat
                            [ beforeOffsetBytes
                            , List.repeat offsetBytes 0
                            ]
                        )

                Err _ ->
                    listValue_Empty

        _ ->
            listValue_Empty


kernelFunction_bit_shift_left_recursive : List Int -> List Int -> Int -> Int -> List Int
kernelFunction_bit_shift_left_recursive bytesRight bytesLeft carryFromRight offset =
    {-
       Recursing from the right to the left.
    -}
    case bytesLeft of
        byte :: rest ->
            let
                shiftedByte =
                    Bitwise.and 0xFF
                        (Bitwise.or
                            (Bitwise.shiftLeftBy offset byte)
                            carryFromRight
                        )

                nextCarry =
                    Bitwise.shiftRightBy (8 - offset) byte
            in
            kernelFunction_bit_shift_left_recursive
                (shiftedByte :: bytesRight)
                rest
                nextCarry
                offset

        [] ->
            bytesRight


kernelFunction_bit_shift_right : Value -> Value
kernelFunction_bit_shift_right value =
    case value of
        ListValue [ shiftValue, BlobValue bytes ] ->
            case intFromValue shiftValue of
                Ok shiftInt ->
                    let
                        offsetBytes =
                            shiftInt // 8

                        offsetBits =
                            shiftInt - (offsetBytes * 8)
                    in
                    BlobValue
                        (kernelFunction_bit_shift_right_recursive
                            []
                            (List.concat
                                [ List.repeat offsetBytes 0
                                , List.reverse (List.drop offsetBytes (List.reverse bytes))
                                ]
                            )
                            0
                            offsetBits
                        )

                Err _ ->
                    listValue_Empty

        _ ->
            listValue_Empty


kernelFunction_bit_shift_right_recursive : List Int -> List Int -> Int -> Int -> List Int
kernelFunction_bit_shift_right_recursive bytesLeft bytesRight carryFromLeft offset =
    {-
       Recursing from the left to the right.
    -}
    case bytesRight of
        byte :: rest ->
            let
                shiftedByte =
                    Bitwise.and 0xFF
                        (Bitwise.or
                            (Bitwise.shiftRightBy offset byte)
                            carryFromLeft
                        )

                nextCarry =
                    Bitwise.shiftLeftBy (8 - offset) byte
            in
            kernelFunction_bit_shift_right_recursive
                (shiftedByte :: bytesLeft)
                rest
                nextCarry
                offset

        [] ->
            List.reverse bytesLeft


bit_and_tuple : List Int -> List Int -> List Int -> List Int
bit_and_tuple merged first second =
    case first of
        [] ->
            merged

        firstFirst :: firstRest ->
            case second of
                [] ->
                    merged

                secondFirst :: secondRest ->
                    bit_and_tuple
                        (Bitwise.and firstFirst secondFirst :: merged)
                        firstRest
                        secondRest


bit_or_tuple : List Int -> List Int -> List Int -> List Int
bit_or_tuple merged first second =
    case first of
        [] ->
            List.concat
                [ List.reverse second
                , merged
                ]

        firstFirst :: firstRest ->
            case second of
                [] ->
                    List.concat
                        [ List.reverse first
                        , merged
                        ]

                secondFirst :: secondRest ->
                    bit_or_tuple
                        (Bitwise.or firstFirst secondFirst :: merged)
                        firstRest
                        secondRest


bit_xor_tuple : List Int -> List Int -> List Int -> List Int
bit_xor_tuple merged first second =
    case first of
        [] ->
            List.concat
                [ List.reverse second
                , merged
                ]

        firstFirst :: firstRest ->
            case second of
                [] ->
                    List.concat
                        [ List.reverse first
                        , merged
                        ]

                secondFirst :: secondRest ->
                    bit_xor_tuple
                        (Bitwise.xor firstFirst secondFirst :: merged)
                        firstRest
                        secondRest


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


int_is_sorted_asc_recursive : List Value -> Int -> Value
int_is_sorted_asc_recursive remaining previous =
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
                        int_is_sorted_asc_recursive rest nextInt


evaluateParseAndEval : EvalEnvironment -> ( Expression, Expression ) -> Result (PathDescription String) Value
evaluateParseAndEval context ( encodedExpr, envExpr ) =
    case evaluateExpression context envExpr of
        Err error ->
            Err
                (DescribePathNode
                    ("Failed to evaluate environment '"
                        ++ describeExpression 1 envExpr
                        ++ "'"
                    )
                    error
                )

        Ok environmentValue ->
            case evaluateExpression context encodedExpr of
                Err error ->
                    Err
                        (DescribePathNode
                            ("Failed to evaluate expression '"
                                ++ describeExpression 1 encodedExpr
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
                            evaluateExpression (EvalEnvironment environmentValue) functionExpression


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
                        String.join ","
                            (describeExpressionList
                                []
                                (depthLimit - 1)
                                list
                            )
                            ++ "]"
                   )

        LiteralExpression literal ->
            "literal("
                ++ describeValue (depthLimit - 1) literal
                ++ ")"

        ParseAndEvalExpression _ exprExpr ->
            "parse-and-eval("
                ++ (if depthLimit < 1 then
                        "..."

                    else
                        describeExpression (depthLimit - 1) exprExpr
                            ++ ")"
                   )

        KernelApplicationExpression functionName _ ->
            "kernel-application("
                ++ functionName
                ++ ")"

        ConditionalExpression _ _ _ ->
            "conditional"

        EnvironmentExpression ->
            "environment"

        StringTagExpression tag tagged ->
            "string-tag-"
                ++ tag
                ++ "("
                ++ describeExpression (depthLimit - 1) tagged
                ++ ")"


describeExpressionList : List String -> Int -> List Expression -> List String
describeExpressionList acc depthLimit expressions =
    case expressions of
        expr :: rest ->
            describeExpressionList
                (describeExpression depthLimit expr :: acc)
                depthLimit
                rest

        [] ->
            List.reverse acc


describeValue : Int -> Value -> String
describeValue maxDepth value =
    case value of
        BlobValue blob ->
            "BlobValue ["
                ++ String.fromInt (List.length blob)
                ++ "] 0x"
                ++ hexadecimalStringFromBlobValue [] blob

        ListValue list ->
            let
                standard : String
                standard =
                    "["
                        ++ (if maxDepth < 0 then
                                "..."

                            else
                                String.join ", "
                                    (describeValueList
                                        []
                                        (maxDepth - 1)
                                        list
                                    )
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


describeValueList : List String -> Int -> List Value -> List String
describeValueList acc maxDepth values =
    case values of
        value :: rest ->
            describeValueList
                (describeValue maxDepth value :: acc)
                maxDepth
                rest

        _ ->
            List.reverse acc


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
        "equal" ->
            stringAsValue_equal

        "head" ->
            stringAsValue_head

        "skip" ->
            stringAsValue_skip

        "String" ->
            stringAsValue_String

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
    let
        charsBytes : List (List Int)
        charsBytes =
            blobBytesFromChars [] (String.toList string)
    in
    BlobValue
        (List.concat charsBytes)


blobBytesFromChars : List (List Int) -> List Char -> List (List Int)
blobBytesFromChars acc remaining =
    case remaining of
        [] ->
            List.reverse acc

        char :: rest ->
            blobBytesFromChars
                (blobBytesFromChar char :: acc)
                rest


valueFromChar : Char -> Value
valueFromChar char =
    BlobValue (blobBytesFromChar char)


blobBytesFromChar : Char -> List Int
blobBytesFromChar char =
    let
        charCode : Int
        charCode =
            Char.toCode char
    in
    [ modBy 0x0100 (charCode // 0x01000000)
    , modBy 0x0100 (charCode // 0x00010000)
    , modBy 0x0100 (charCode // 0x0100)
    , modBy 0x0100 charCode
    ]


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
            BlobValue bytes ->
                stringFromUtf32Bytes [] bytes

            ListValue _ ->
                Err "Only a BlobValue can encode a string."


stringFromUtf32Bytes : List Char -> List Int -> Result String String
stringFromUtf32Bytes chars bytes =
    case List.take 4 (List.drop (List.length chars * 4) bytes) of
        [] ->
            Ok (String.fromList (List.reverse chars))

        [ b1, b2, b3, b4 ] ->
            let
                charCode : Int
                charCode =
                    (b1 * 16777216) + (b2 * 65536) + (b3 * 256) + b4
            in
            stringFromUtf32Bytes
                (Char.fromCode charCode :: chars)
                bytes

        lessBytes ->
            Err
                ("Failed to map to char at index "
                    ++ String.fromInt (List.length chars * 4)
                    ++ " - "
                    ++ String.fromInt (List.length lessBytes)
                    ++ " bytes left"
                )


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
            let
                upper : Int
                upper =
                    remaining // 0x0100
            in
            if upper == 0 then
                remaining :: aggregate

            else
                let
                    lower =
                        remaining - (upper * 0x0100)
                in
                recursiveFromInt upper (lower :: aggregate)
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


hexadecimalStringFromBlobValue : List String -> List Int -> String
hexadecimalStringFromBlobValue acc bytes =
    case bytes of
        byte :: rest ->
            hexadecimalStringFromBlobValue
                (String.padLeft 2 '0' (Hex.toString byte) :: acc)
                rest

        _ ->
            String.concat (List.reverse acc)


encodeExpressionAsValue : Expression -> Value
encodeExpressionAsValue expression =
    case expression of
        LiteralExpression literal ->
            encodeUnionToPineValue
                stringAsValue_Literal
                (ListValue [ literal ])

        ListExpression listExpr ->
            encodeListExpressionAsValueReversed [] (List.reverse listExpr)

        ParseAndEvalExpression encodedExpr envExpr ->
            encodeUnionToPineValue
                stringAsValue_ParseAndEval
                (ListValue
                    [ encodeExpressionAsValue encodedExpr
                    , encodeExpressionAsValue envExpr
                    ]
                )

        KernelApplicationExpression functionName input ->
            encodeUnionToPineValue
                stringAsValue_KernelApplication
                (ListValue
                    [ computeValueFromString functionName
                    , encodeExpressionAsValue input
                    ]
                )

        ConditionalExpression condition falseBranch trueBranch ->
            encodeUnionToPineValue
                stringAsValue_Conditional
                (ListValue
                    [ encodeExpressionAsValue condition
                    , encodeExpressionAsValue falseBranch
                    , encodeExpressionAsValue trueBranch
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
                (ListValue [ ListValue encodedItems ])

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
    case exprValue of
        ListValue (tagNameValue :: (ListValue tagArguments) :: _) ->
            case stringFromValue tagNameValue of
                Err err ->
                    Err ("Failed parsing tag name: " ++ err)

                Ok tagName ->
                    case tagName of
                        "Literal" ->
                            case tagArguments of
                                argument :: _ ->
                                    Ok (LiteralExpression argument)

                                [] ->
                                    Err "Expected one argument for literal but got zero"

                        "List" ->
                            case tagArguments of
                                argument :: _ ->
                                    parseListExpression argument

                                [] ->
                                    Err "Expected one argument for list but got zero"

                        "ParseAndEval" ->
                            case parseParseAndEvalExpression tagArguments of
                                Ok ( encodedExpr, envExpr ) ->
                                    Ok (ParseAndEvalExpression encodedExpr envExpr)

                                Err err ->
                                    Err err

                        "KernelApplication" ->
                            case parseKernelApplicationExpression tagArguments of
                                Ok ( functionName, input ) ->
                                    Ok (KernelApplicationExpression functionName input)

                                Err err ->
                                    Err err

                        "Conditional" ->
                            case parseConditionalExpression tagArguments of
                                Ok ( condition, falseBranch, trueBranch ) ->
                                    Ok (ConditionalExpression condition falseBranch trueBranch)

                                Err err ->
                                    Err err

                        "Environment" ->
                            Ok environmentExpr

                        "StringTag" ->
                            parseStringTagExpression tagArguments

                        _ ->
                            Err ("Unexpected expr tag: " ++ tagName)

        ListValue list ->
            Err
                ("Unexpected shape in list value ("
                    ++ String.fromInt (List.length list)
                    ++ " items)"
                )

        BlobValue _ ->
            Err "Unexpected blob value"


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


parseParseAndEvalExpression : List Value -> Result String ( Expression, Expression )
parseParseAndEvalExpression arguments =
    case arguments of
        encodedValue :: envValue :: _ ->
            case parseExpressionFromValue encodedValue of
                Err parseErr ->
                    Err ("Failed to parse encoded field: " ++ parseErr)

                Ok encodedExpr ->
                    case parseExpressionFromValue envValue of
                        Err envErr ->
                            Err ("Failed to parse env field: " ++ envErr)

                        Ok envExpr ->
                            Ok ( encodedExpr, envExpr )

        list ->
            Err
                ("Failed to parse parse-and-eval: Too few elements in top list or unexpected shape of fields ("
                    ++ String.fromInt (List.length list)
                    ++ ")"
                )


parseKernelApplicationExpression : List Value -> Result String ( String, Expression )
parseKernelApplicationExpression arguments =
    case arguments of
        functionNameValue :: inputValue :: _ ->
            case stringFromValue functionNameValue of
                Err error ->
                    Err ("Failed to parse kernel application function name: " ++ error)

                Ok functionName ->
                    case parseExpressionFromValue inputValue of
                        Err error ->
                            Err ("Failed to parse kernel application input: " ++ error)

                        Ok input ->
                            Ok ( functionName, input )

        list ->
            Err
                ("Failed to parse kernel application expression: Too few arguments ("
                    ++ String.fromInt (List.length list)
                    ++ ")"
                )


parseConditionalExpression : List Value -> Result String ( Expression, Expression, Expression )
parseConditionalExpression arguments =
    case arguments of
        [ conditionValue, falseBranchValue, trueBranchValue ] ->
            case parseExpressionFromValue conditionValue of
                Err error ->
                    Err ("Failed to parse condition: " ++ error)

                Ok condition ->
                    case parseExpressionFromValue falseBranchValue of
                        Err error ->
                            Err ("Failed to parse false branch: " ++ error)

                        Ok falseBranch ->
                            case parseExpressionFromValue trueBranchValue of
                                Err error ->
                                    Err ("Failed to parse true branch: " ++ error)

                                Ok trueBranch ->
                                    Ok ( condition, falseBranch, trueBranch )

        list ->
            Err
                ("Failed to parse conditional: Wrong number of arguments ("
                    ++ String.fromInt (List.length list)
                    ++ ")"
                )


parseStringTagExpression : List Value -> Result String Expression
parseStringTagExpression arguments =
    case arguments of
        tagValue :: taggedValue :: _ ->
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
            Err
                ("Unexpected shape under string tag ("
                    ++ String.fromInt (List.length arguments)
                    ++ " items)"
                )


{-| Returns aggregate node count and aggregate blob byte count.
-}
countValueContent : Value -> ( Int, Int )
countValueContent value =
    case value of
        BlobValue blob ->
            ( 0, List.length blob )

        ListValue list ->
            countListValueContent ( 0, 0 ) list


countListValueContent : ( Int, Int ) -> List Value -> ( Int, Int )
countListValueContent ( nodeCount, byteCount ) items =
    case items of
        [] ->
            ( nodeCount, byteCount )

        item :: remaining ->
            let
                ( itemNodeCount, itemByteCount ) =
                    countValueContent item
            in
            countListValueContent
                ( nodeCount + itemNodeCount + 1
                , byteCount + itemByteCount
                )
                remaining


encodeUnionToPineValue : Value -> Value -> Value
encodeUnionToPineValue tagNameValue unionTagValue =
    ListValue [ tagNameValue, unionTagValue ]


valueFromContextExpansionWithName : ( String, Value ) -> Value
valueFromContextExpansionWithName ( declName, declValue ) =
    ListValue [ computeValueFromString declName, declValue ]


environmentExpr : Expression
environmentExpr =
    EnvironmentExpression


stringAsValue_equal : Value
stringAsValue_equal =
    computeValueFromString "equal"


stringAsValue_head : Value
stringAsValue_head =
    computeValueFromString "head"


stringAsValue_skip : Value
stringAsValue_skip =
    computeValueFromString "skip"


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


stringAsValue_String : Value
stringAsValue_String =
    computeValueFromString "String"


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
