module Pine exposing
    ( Expression(..)
    , Value(..)
    , applyBuiltinFunction
    , bigIntFromValue
    , encodeExpressionAsValue
    , evalExpression
    , falseValue
    , parseExpressionFromValue
    , stringFromValue
    , trueValue
    , valueFromBigInt
    , valueFromBool
    , valueFromInt
    , valueFromString
    )

{-| Pine expressions, their 2026 encoding, and a direct interpreter.

Blob bytes are represented as lists of integers in the range 0 through 255.
Labels are arbitrary Pine values and have no effect on evaluation.

-}

import BigInt
import Bitwise


type Value
    = BlobValue (List Int)
    | ListValue (List Value)


type Expression
    = LitralExpression Value
    | ListExpression (List Expression)
    | EvalExpression Expression Expression
    | BuiltinExpression String Expression
    | ConditionalExpression Expression Expression Expression
    | EnvironmentExpression
    | LabelExpression Value Expression


encodeExpressionAsValue : Expression -> Value
encodeExpressionAsValue expression =
    ListValue
        (case expression of
            LitralExpression value ->
                [ valueFromString "Litral", value ]

            ListExpression items ->
                valueFromString "List" :: List.map encodeExpressionAsValue items

            EvalExpression encoded environment ->
                [ valueFromString "Eval"
                , encodeExpressionAsValue encoded
                , encodeExpressionAsValue environment
                ]

            BuiltinExpression function input ->
                [ valueFromString "Builtin"
                , valueFromString function
                , encodeExpressionAsValue input
                ]

            ConditionalExpression condition falseBranch trueBranch ->
                [ valueFromString "Conditional"
                , encodeExpressionAsValue condition
                , encodeExpressionAsValue falseBranch
                , encodeExpressionAsValue trueBranch
                ]

            EnvironmentExpression ->
                [ valueFromString "Environment" ]

            LabelExpression label tagged ->
                [ valueFromString "Label", label, encodeExpressionAsValue tagged ]
        )


parseExpressionFromValue : Value -> Result String Expression
parseExpressionFromValue value =
    case value of
        BlobValue _ ->
            Err "An encoded expression must be a nonempty list."

        ListValue [] ->
            Err "An encoded expression must be a nonempty list."

        ListValue (tag :: arguments) ->
            stringFromValue tag
                |> Result.mapError ((++) "Failed to decode expression tag: ")
                |> Result.andThen
                    (\name ->
                        case ( name, arguments ) of
                            ( "Litral", [ literal ] ) ->
                                Ok (LitralExpression literal)

                            ( "List", items ) ->
                                parseExpressionList 0 [] items
                                    |> Result.map ListExpression

                            ( "Eval", [ encoded, environment ] ) ->
                                Result.map2 EvalExpression
                                    (parseChild "Eval encoded expression" encoded)
                                    (parseChild "Eval environment" environment)

                            ( "Builtin", [ function, input ] ) ->
                                Result.map2 BuiltinExpression
                                    (stringFromValue function
                                        |> Result.mapError ((++) "Failed to decode builtin name: ")
                                    )
                                    (parseChild "Builtin input" input)

                            ( "Conditional", [ condition, falseBranch, trueBranch ] ) ->
                                parseConditional condition falseBranch trueBranch

                            ( "Condition", [ condition, falseBranch, trueBranch ] ) ->
                                parseConditional condition falseBranch trueBranch

                            ( "Environment", [] ) ->
                                Ok EnvironmentExpression

                            ( "Label", [ label, tagged ] ) ->
                                parseChild "Label expression" tagged
                                    |> Result.map (LabelExpression label)

                            _ ->
                                Err
                                    ("Unknown expression tag or invalid argument count: "
                                        ++ name
                                        ++ " ("
                                        ++ String.fromInt (List.length arguments)
                                        ++ " arguments)"
                                    )
                    )


parseConditional : Value -> Value -> Value -> Result String Expression
parseConditional condition falseBranch trueBranch =
    Result.map3 ConditionalExpression
        (parseChild "Conditional condition" condition)
        (parseChild "Conditional false branch" falseBranch)
        (parseChild "Conditional true branch" trueBranch)


parseChild : String -> Value -> Result String Expression
parseChild context value =
    parseExpressionFromValue value
        |> Result.mapError (\error -> context ++ ": " ++ error)


parseExpressionList : Int -> List Expression -> List Value -> Result String (List Expression)
parseExpressionList index reversed remaining =
    case remaining of
        [] ->
            Ok (List.reverse reversed)

        item :: rest ->
            case parseChild ("List item " ++ String.fromInt index) item of
                Ok parsed ->
                    parseExpressionList (index + 1) (parsed :: reversed) rest

                Err error ->
                    Err error


evalExpression : Expression -> Value -> Result String Value
evalExpression expression environment =
    case expression of
        LitralExpression value ->
            Ok value

        ListExpression items ->
            evalExpressionList environment 0 [] items
                |> Result.map ListValue

        EnvironmentExpression ->
            Ok environment

        BuiltinExpression function input ->
            evalExpression input environment
                |> Result.mapError ((++) ("Builtin " ++ function ++ " input: "))
                |> Result.andThen (applyBuiltinFunction function)

        ConditionalExpression condition falseBranch trueBranch ->
            evalExpression condition environment
                |> Result.mapError ((++) "Conditional condition: ")
                |> Result.andThen
                    (\value ->
                        if value == trueValue then
                            evalExpression trueBranch environment

                        else
                            evalExpression falseBranch environment
                    )

        EvalExpression encoded environmentExpression ->
            evalExpression environmentExpression environment
                |> Result.mapError ((++) "Eval environment: ")
                |> Result.andThen
                    (\innerEnvironment ->
                        evalExpression encoded environment
                            |> Result.mapError ((++) "Eval encoded expression: ")
                            |> Result.andThen
                                (\encodedValue ->
                                    parseExpressionFromValue encodedValue
                                        |> Result.mapError ((++) "Eval decode: ")
                                )
                            |> Result.andThen (\inner -> evalExpression inner innerEnvironment)
                    )

        LabelExpression _ tagged ->
            evalExpression tagged environment


evalExpressionList : Value -> Int -> List Value -> List Expression -> Result String (List Value)
evalExpressionList environment index reversed remaining =
    case remaining of
        [] ->
            Ok (List.reverse reversed)

        item :: rest ->
            case evalExpression item environment of
                Ok value ->
                    evalExpressionList environment (index + 1) (value :: reversed) rest

                Err error ->
                    Err ("List item " ++ String.fromInt index ++ ": " ++ error)


applyBuiltinFunction : String -> Value -> Result String Value
applyBuiltinFunction name input =
    case name of
        "equal" ->
            Ok
                (valueFromBool
                    (case input of
                        BlobValue bytes ->
                            allEqual bytes

                        ListValue items ->
                            allEqual items
                    )
                )

        "negate" ->
            Ok
                (case input of
                    BlobValue (4 :: rest) ->
                        BlobValue (2 :: rest)

                    BlobValue (2 :: rest) ->
                        BlobValue (4 :: rest)

                    _ ->
                        ListValue []
                )

        "length" ->
            Ok (valueFromInt (sequenceLength input))

        "head" ->
            Ok
                (case input of
                    ListValue items ->
                        Maybe.withDefault (ListValue []) (List.head items)

                    BlobValue bytes ->
                        BlobValue (List.take 1 bytes)
                )

        "skip" ->
            Ok (sliceSequence True input)

        "take" ->
            Ok (sliceSequence False input)

        "reverse" ->
            Ok (mapSequence List.reverse List.reverse input)

        "concat" ->
            Ok (concatSequences input)

        "int_add" ->
            Ok (aggregateIntegers BigInt.add (BigInt.fromInt 0) input)

        "int_mul" ->
            Ok (aggregateIntegers BigInt.mul (BigInt.fromInt 1) input)

        "int_is_sorted_asc" ->
            Ok (integersAreSorted input)

        "bit_and" ->
            Ok (combineBlobs False Bitwise.and input)

        "bit_or" ->
            Ok (combineBlobs True Bitwise.or input)

        "bit_xor" ->
            Ok (combineBlobs True Bitwise.xor input)

        "bit_not" ->
            Ok
                (case input of
                    BlobValue bytes ->
                        BlobValue (List.map (\byte -> Bitwise.xor 255 byte) bytes)

                    ListValue _ ->
                        ListValue []
                )

        "bit_shift_left" ->
            Ok (shiftBlob True input)

        "bit_shift_right" ->
            Ok (shiftBlob False input)

        _ ->
            Err ("Unknown builtin function: " ++ name)


allEqual : List a -> Bool
allEqual items =
    case items of
        [] ->
            True

        first :: rest ->
            List.all ((==) first) rest


sequenceLength : Value -> Int
sequenceLength value =
    case value of
        BlobValue bytes ->
            List.length bytes

        ListValue items ->
            List.length items


mapSequence : (List Int -> List Int) -> (List Value -> List Value) -> Value -> Value
mapSequence mapBlob mapList value =
    case value of
        BlobValue bytes ->
            BlobValue (mapBlob bytes)

        ListValue items ->
            ListValue (mapList items)


sliceSequence : Bool -> Value -> Value
sliceSequence drop input =
    case input of
        ListValue [ countValue, sequence ] ->
            case boundedCount (sequenceLength sequence) countValue of
                Ok count ->
                    if drop then
                        mapSequence (List.drop count) (List.drop count) sequence

                    else
                        mapSequence (List.take count) (List.take count) sequence

                Err _ ->
                    ListValue []

        _ ->
            ListValue []


boundedCount : Int -> Value -> Result String Int
boundedCount limit value =
    bigIntFromValue value
        |> Result.map
            (\integer ->
                if BigInt.lt integer (BigInt.fromInt 0) then
                    0

                else if BigInt.gte integer (BigInt.fromInt limit) then
                    limit

                else
                    -- The comparison above guarantees this is a small nonnegative integer.
                    decimalDigitsToInt (BigInt.toString integer)
            )


concatSequences : Value -> Value
concatSequences input =
    case input of
        ListValue items ->
            case List.filter ((/=) (ListValue [])) items of
                [] ->
                    ListValue []

                (BlobValue _) :: _ ->
                    collectBlobs [] items
                        |> Maybe.map (List.concat >> BlobValue)
                        |> Maybe.withDefault (ListValue [])

                _ ->
                    collectLists [] items
                        |> Maybe.map (List.concat >> ListValue)
                        |> Maybe.withDefault (ListValue [])

        BlobValue _ ->
            ListValue []


collectBlobs : List (List Int) -> List Value -> Maybe (List (List Int))
collectBlobs reversed remaining =
    case remaining of
        [] ->
            Just (List.reverse reversed)

        (BlobValue bytes) :: rest ->
            collectBlobs (bytes :: reversed) rest

        (ListValue []) :: rest ->
            collectBlobs reversed rest

        _ ->
            Nothing


collectLists : List (List Value) -> List Value -> Maybe (List (List Value))
collectLists reversed remaining =
    case remaining of
        [] ->
            Just (List.reverse reversed)

        (ListValue items) :: rest ->
            collectLists (items :: reversed) rest

        _ ->
            Nothing


aggregateIntegers : (BigInt.BigInt -> BigInt.BigInt -> BigInt.BigInt) -> BigInt.BigInt -> Value -> Value
aggregateIntegers combine seed input =
    case input of
        ListValue items ->
            aggregateIntegerList combine seed items

        BlobValue _ ->
            ListValue []


aggregateIntegerList : (BigInt.BigInt -> BigInt.BigInt -> BigInt.BigInt) -> BigInt.BigInt -> List Value -> Value
aggregateIntegerList combine aggregate remaining =
    case remaining of
        [] ->
            valueFromBigInt aggregate

        item :: rest ->
            case bigIntFromValue item of
                Ok integer ->
                    aggregateIntegerList combine (combine aggregate integer) rest

                Err _ ->
                    ListValue []


integersAreSorted : Value -> Value
integersAreSorted input =
    case input of
        ListValue [] ->
            trueValue

        ListValue (first :: rest) ->
            case bigIntFromValue first of
                Ok integer ->
                    integerListIsSorted integer rest

                Err _ ->
                    ListValue []

        BlobValue _ ->
            ListValue []


integerListIsSorted : BigInt.BigInt -> List Value -> Value
integerListIsSorted previous remaining =
    case remaining of
        [] ->
            trueValue

        item :: rest ->
            case bigIntFromValue item of
                Ok integer ->
                    if BigInt.lt integer previous then
                        falseValue

                    else
                        integerListIsSorted integer rest

                Err _ ->
                    ListValue []


combineBlobs : Bool -> (Int -> Int -> Int) -> Value -> Value
combineBlobs pad combine input =
    case input of
        ListValue ((BlobValue first) :: rest) ->
            combineBlobList pad combine first rest

        _ ->
            ListValue []


combineBlobList : Bool -> (Int -> Int -> Int) -> List Int -> List Value -> Value
combineBlobList pad combine aggregate remaining =
    case remaining of
        [] ->
            BlobValue aggregate

        (BlobValue bytes) :: rest ->
            let
                width =
                    if pad then
                        max (List.length aggregate) (List.length bytes)

                    else
                        min (List.length aggregate) (List.length bytes)

                align value =
                    List.repeat (max 0 (width - List.length value)) 0
                        ++ List.drop (max 0 (List.length value - width)) value
            in
            combineBlobList pad
                combine
                (List.map2 combine (align aggregate) (align bytes))
                rest

        _ ->
            ListValue []


shiftBlob : Bool -> Value -> Value
shiftBlob left input =
    case input of
        ListValue [ countValue, BlobValue bytes ] ->
            case boundedCount (List.length bytes * 8) countValue of
                Ok count ->
                    let
                        byteOffset =
                            count // 8

                        bitOffset =
                            modBy 8 count

                        shifted =
                            if left then
                                shiftBytesLeft bitOffset 0 [] (List.reverse (List.drop byteOffset bytes))
                                    ++ List.repeat byteOffset 0

                            else
                                List.repeat byteOffset 0
                                    ++ List.reverse
                                        (shiftBytesRight bitOffset
                                            0
                                            []
                                            (List.take (List.length bytes - byteOffset) bytes)
                                        )
                    in
                    BlobValue shifted

                Err _ ->
                    ListValue []

        _ ->
            ListValue []


shiftBytesLeft : Int -> Int -> List Int -> List Int -> List Int
shiftBytesLeft offset carry result remaining =
    case remaining of
        [] ->
            result

        byte :: rest ->
            shiftBytesLeft offset
                (Bitwise.shiftRightZfBy (8 - offset) byte)
                (Bitwise.and 255 (Bitwise.or carry (Bitwise.shiftLeftBy offset byte)) :: result)
                rest


shiftBytesRight : Int -> Int -> List Int -> List Int -> List Int
shiftBytesRight offset carry result remaining =
    case remaining of
        [] ->
            result

        byte :: rest ->
            shiftBytesRight offset
                (Bitwise.and 255 (Bitwise.shiftLeftBy (8 - offset) byte))
                (Bitwise.or carry (Bitwise.shiftRightZfBy offset byte) :: result)
                rest


trueValue : Value
trueValue =
    BlobValue [ 4 ]


falseValue : Value
falseValue =
    BlobValue [ 2 ]


valueFromBool : Bool -> Value
valueFromBool value =
    if value then
        trueValue

    else
        falseValue


valueFromInt : Int -> Value
valueFromInt integer =
    valueFromBigInt (BigInt.fromInt integer)


valueFromBigInt : BigInt.BigInt -> Value
valueFromBigInt integer =
    let
        sign =
            if BigInt.lt integer (BigInt.fromInt 0) then
                2

            else
                4
    in
    BlobValue (sign :: unsignedIntegerBytes (BigInt.abs integer))


unsignedIntegerBytes : BigInt.BigInt -> List Int
unsignedIntegerBytes integer =
    String.foldl
        (\digit bytes ->
            multiplyBytesByTen (Char.toCode digit - Char.toCode '0') [] (List.reverse bytes)
        )
        [ 0 ]
        (BigInt.toString integer)


multiplyBytesByTen : Int -> List Int -> List Int -> List Int
multiplyBytesByTen carry result remaining =
    case remaining of
        [] ->
            if carry == 0 then
                result

            else
                carry :: result

        byte :: rest ->
            let
                product =
                    byte * 10 + carry
            in
            multiplyBytesByTen (product // 256) (modBy 256 product :: result) rest


decimalDigitsToInt : String -> Int
decimalDigitsToInt digits =
    String.foldl
        (\digit aggregate -> aggregate * 10 + Char.toCode digit - Char.toCode '0')
        0
        digits


bigIntFromValue : Value -> Result String BigInt.BigInt
bigIntFromValue value =
    case value of
        BlobValue (sign :: first :: rest) ->
            if (sign == 2 || sign == 4) && List.all validByte (first :: rest) then
                let
                    magnitude =
                        unsignedIntegerFromBytes (first :: rest)
                in
                Ok
                    (if sign == 2 then
                        BigInt.negate magnitude

                     else
                        magnitude
                    )

            else
                Err "Invalid signed integer sign or magnitude bytes."

        _ ->
            Err "A signed integer requires a sign byte and at least one magnitude byte."


unsignedIntegerFromBytes : List Int -> BigInt.BigInt
unsignedIntegerFromBytes bytes =
    List.foldl
        (\byte aggregate ->
            BigInt.add (BigInt.mul aggregate (BigInt.fromInt 256)) (BigInt.fromInt byte)
        )
        (BigInt.fromInt 0)
        bytes


validByte : Int -> Bool
validByte byte =
    byte >= 0 && byte <= 255


valueFromString : String -> Value
valueFromString text =
    BlobValue
        (String.toList text
            |> List.concatMap
                (\char ->
                    let
                        code =
                            Char.toCode char
                    in
                    [ code // 16777216
                    , modBy 256 (code // 65536)
                    , modBy 256 (code // 256)
                    , modBy 256 code
                    ]
                )
        )


stringFromValue : Value -> Result String String
stringFromValue value =
    case value of
        BlobValue bytes ->
            stringFromUtf32Bytes [] bytes

        ListValue _ ->
            Err "A string must be a UTF-32 big-endian blob."


stringFromUtf32Bytes : List Char -> List Int -> Result String String
stringFromUtf32Bytes reversed remaining =
    case remaining of
        [] ->
            Ok (String.fromList (List.reverse reversed))

        a :: b :: c :: d :: rest ->
            let
                code =
                    a * 16777216 + b * 65536 + c * 256 + d
            in
            if List.all validByte [ a, b, c, d ] && code <= 1114111 && not (code >= 55296 && code <= 57343) then
                stringFromUtf32Bytes (Char.fromCode code :: reversed) rest

            else
                Err "Invalid Unicode scalar in UTF-32 string."

        _ ->
            Err "UTF-32 string length must be a multiple of four bytes."
