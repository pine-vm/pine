module Optimizations exposing (..)

import Array
import Bitwise
import Dict


results : List String
results =
    [ String.fromInt (tailIf 8 0)
    , String.fromInt (tailCase [ 1, 2, 3 ] 0)
    , String.fromInt (tailLet 8 0)
    , String.fromInt (tailDestructure 5 ( 1, 2 ))
    , String.fromInt (tailBranches 8 0)
    , showPair (tailSwap 5 1 2)
    , String.fromInt (tailRecord { remaining = 5, total = 0 })
    , String.fromInt (tailArgumentPattern 5 ( 1, 2 ))
    , String.fromInt (tailLocalBinding 5 0)
    , String.fromInt (localTail [ 1, 2, 3 ])
    , String.fromInt (localMutual 7)
    , String.fromInt (nonTail 5)
    , String.fromInt (nonTailBinding 5)
    , String.fromInt (nonTailWrapped 5)
    , String.fromInt (nonTailLambda 5)
    , String.fromInt (tailPipeLeft 5)
    , String.fromInt (tailPipeRight 5)
    , showBools [ mutualEven 6, mutualOdd 7 ]
    , showTripleInt (numericInt 17 3)
    , showTripleFloat (numericFloat 7.5 2.5)
    , showBools (booleanOps True False)
    , showBools (literalEquality 3 False)
    , showBools (literalStrings "other")
    , showBools (literalFloats 1.5)
    , showBools (characterEquality 'a')
    , showBools (genericEquality [ 1, 2 ] [ 1, 3 ])
    , showBools (literalComparisons 3)
    , showBools (genericComparisons [ 1, 2 ] [ 1, 3 ])
    , appendStrings "a" "b" "c"
    , appendLeftNested "a" "b" "c"
    , appendUnknownStrings "left" "right"
    , showInts (appendLists [ 1 ] [ 2 ] [ 3 ])
    , String.fromInt (tupleAccess ( 3, 4 ))
    , showInts (bitwiseOps 17 3)
    , String.fromInt (Maybe.withDefault -1 (arrayOps 1 9))
    , String.fromInt (dictionaryOps 3)
    , showTripleInt ( pipeLeft 5, pipeRight 5, accessorPipe { amount = 5 } )
    , String.fromInt (pipeBare 5)
    , String.fromInt (prefixArithmetic 3 4)
    , showPair ( composeLeft 5, composeRight 5 )
    , String.fromInt (arity2 1 2)
    , String.fromInt (arity3 1 2 3)
    , String.fromInt (arity4 1 2 3 4)
    , String.fromInt (arity5 1 2 3 4 5)
    , String.fromInt (arity6 1 2 3 4 5 6)
    , String.fromInt (arity7 1 2 3 4 5 6 7)
    , String.fromInt (arity8 1 2 3 4 5 6 7 8)
    , String.fromInt (arity9 1 2 3 4 5 6 7 8 9)
    , String.fromInt (arity10 1 2 3 4 5 6 7 8 9 10)
    , String.fromInt (partialCall 5)
    , String.fromInt (partialCoreCall 5)
    , String.fromInt (partialBasicsCall 5)
    , caseFanout 2
    , String.fromInt (decisionShared ( Red, Blue ))
    , String.fromInt (unwrap (Wrapped 7))
    , String.fromInt (wrappedAsValue 8)
    , String.fromInt (enumPattern Green)
    , String.fromInt (normalPattern (Pair 3 4))
    , String.fromInt (normalPattern Empty)
    , String.fromInt (recordSpecializations { longFieldName = 7, otherFieldName = 3 })
    , String.fromInt (characterPattern 'a')
    , String.fromInt (characterPattern 'b')
    , String.fromInt (booleanPattern True)
    , String.fromInt (stringPattern "match")
    , String.fromInt (unitPattern ())
    , String.fromInt (conditionalExpression 3)
    , String.fromInt (conditionalBlock 3)
    , String.fromChar (Tuple.second (literalRepresentations ()))
    , showInts (emptyList ())
    ]


showInts : List Int -> String
showInts values =
    String.join "," (List.map String.fromInt values)


showBools : List Bool -> String
showBools values =
    String.join ","
        (List.map
            (\value ->
                if value then
                    "true"

                else
                    "false"
            )
            values
        )


showPair : ( Int, Int ) -> String
showPair ( left, right ) =
    showInts [ left, right ]


showTripleInt : ( Int, Int, Int ) -> String
showTripleInt ( first, second, third ) =
    showInts [ first, second, third ]


showTripleFloat : ( Float, Float, Float ) -> String
showTripleFloat ( first, second, third ) =
    String.join "," (List.map String.fromFloat [ first, second, third ])


tailIf : Int -> Int -> Int
tailIf remaining total =
    if remaining <= 0 then
        total

    else
        tailIf (remaining - 1) (total + remaining)


tailCase : List Int -> Int -> Int
tailCase values total =
    case values of
        [] ->
            total

        head :: rest ->
            tailCase rest (total + head)


tailLet : Int -> Int -> Int
tailLet remaining total =
    if remaining <= 0 then
        total

    else
        let
            next =
                remaining - 1

            accumulated =
                total + remaining
        in
        tailLet next accumulated


tailDestructure : Int -> ( Int, Int ) -> Int
tailDestructure remaining pair =
    if remaining <= 0 then
        Tuple.first pair

    else
        let
            ( left, right ) =
                pair
        in
        tailDestructure (remaining - 1) ( right, left + right )


tailBranches : Int -> Int -> Int
tailBranches remaining total =
    if remaining < 0 then
        total

    else if remaining == 0 then
        total + 1

    else if remainderBy 2 remaining == 0 then
        tailBranches (remaining - 1) (total + 2)

    else
        tailBranches (remaining - 2) (total + 3)


tailSwap : Int -> Int -> Int -> ( Int, Int )
tailSwap remaining left right =
    if remaining <= 0 then
        ( left, right )

    else
        tailSwap (remaining - 1) right left


tailRecord : { remaining : Int, total : Int } -> Int
tailRecord ({ remaining, total } as model) =
    if remaining <= 0 then
        total

    else
        tailRecord { model | remaining = remaining - 1, total = total + remaining }


tailArgumentPattern : Int -> ( Int, Int ) -> Int
tailArgumentPattern remaining (( left, right ) as pair) =
    if remaining <= 0 then
        Tuple.first pair

    else
        tailArgumentPattern (remaining - 1) ( right, left + right )


tailLocalBinding : Int -> Int -> Int
tailLocalBinding remaining total =
    let
        adjust number =
            if number <= 0 then
                1

            else
                adjust (number - 1)
    in
    if remaining <= 0 then
        total

    else
        tailLocalBinding (remaining - 1) (total + adjust remaining)


localTail : List Int -> Int
localTail values =
    let
        loop items total =
            case items of
                [] ->
                    total

                head :: rest ->
                    loop rest (total + head)
    in
    loop values 0


localMutual : Int -> Int
localMutual count =
    let
        even remaining =
            if remaining <= 0 then
                1

            else
                odd (remaining - 1)

        odd remaining =
            if remaining <= 0 then
                0

            else
                even (remaining - 1)
    in
    even count


nonTail : Int -> Int
nonTail remaining =
    if remaining <= 0 then
        0

    else
        1 + nonTail (remaining - 1)


nonTailBinding : Int -> Int
nonTailBinding remaining =
    if remaining <= 0 then
        0

    else
        let
            nested =
                nonTailBinding (remaining - 1)
        in
        nested + 1


nonTailWrapped : Int -> Int
nonTailWrapped remaining =
    if remaining <= 0 then
        0

    else
        identity (nonTailWrapped (remaining - 1))


nonTailLambda : Int -> Int
nonTailLambda remaining =
    if remaining <= 0 then
        0

    else
        (\() -> nonTailLambda (remaining - 1)) ()


tailPipeLeft : Int -> Int
tailPipeLeft remaining =
    if remaining <= 0 then
        0

    else
        tailPipeLeft <| remaining - 1


tailPipeRight : Int -> Int
tailPipeRight remaining =
    if remaining <= 0 then
        0

    else
        remaining - 1 |> tailPipeRight


mutualEven : Int -> Bool
mutualEven remaining =
    if remaining <= 0 then
        True

    else
        mutualOdd (remaining - 1)


mutualOdd : Int -> Bool
mutualOdd remaining =
    if remaining <= 0 then
        False

    else
        mutualEven (remaining - 1)


numericInt : Int -> Int -> ( Int, Int, Int )
numericInt left right =
    ( left + right * 3 - 2, left // right, remainderBy right left )


numericFloat : Float -> Float -> ( Float, Float, Float )
numericFloat left right =
    ( left / right, toFloat (truncate left), negate right )


booleanOps : Bool -> Bool -> List Bool
booleanOps left right =
    [ not left, left && right, left || right, xor left right ]


literalEquality : Int -> Bool -> List Bool
literalEquality number flag =
    [ number == 0
    , 0 == number
    , number /= 0
    , 0 /= number
    , flag == True
    , True == flag
    , flag /= True
    , True /= flag
    , flag == False
    , False == flag
    , flag /= False
    , False /= flag
    , number == 7
    , number /= 7
    ]


literalStrings : String -> List Bool
literalStrings value =
    [ value == "text", value /= "text", value < "text", value >= "text" ]


literalFloats : Float -> List Bool
literalFloats value =
    [ value == 1.5, value /= 1.5, value < 1.5, value >= 1.5 ]


characterEquality : Char -> List Bool
characterEquality value =
    [ value == 'x', value /= 'x' ]


genericEquality : a -> a -> List Bool
genericEquality left right =
    [ left == right, left /= right ]


literalComparisons : Int -> List Bool
literalComparisons number =
    [ number < 3
    , 3 < number
    , number <= 3
    , 3 <= number
    , number > 3
    , 3 > number
    , number >= 3
    , 3 >= number
    ]


genericComparisons : comparable -> comparable -> List Bool
genericComparisons left right =
    [ left < right, left <= right, left > right, left >= right ]


appendStrings : String -> String -> String -> String
appendStrings left middle right =
    left ++ middle ++ ":" ++ right


appendLeftNested : String -> String -> String -> String
appendLeftNested left middle right =
    (left ++ ":") ++ (middle ++ right)


appendUnknownStrings : String -> String -> String
appendUnknownStrings left right =
    left ++ right


appendLists : List Int -> List Int -> List Int -> List Int
appendLists left middle right =
    left ++ middle ++ right


tupleAccess : ( Int, Int ) -> Int
tupleAccess pair =
    Tuple.first pair + Tuple.second pair


bitwiseOps : Int -> Int -> List Int
bitwiseOps left right =
    [ Bitwise.and left right
    , Bitwise.or left right
    , Bitwise.xor left right
    , Bitwise.complement left
    , Bitwise.shiftLeftBy right left
    , Bitwise.shiftRightBy right left
    , Bitwise.shiftRightZfBy right left
    ]


arrayOps : Int -> Int -> Maybe Int
arrayOps index value =
    Array.get index (Array.push value (Array.fromList [ value, value + 1 ]))


dictionaryOps : Int -> Int
dictionaryOps number =
    Dict.get number (Dict.insert number 7 Dict.empty)
        |> Maybe.withDefault 0


pipeLeft : Int -> Int
pipeLeft number =
    arity2 number <| 3


pipeRight : Int -> Int
pipeRight number =
    number |> arity2 3


accessorPipe : { amount : Int } -> Int
accessorPipe record =
    record |> .amount


pipeBare : Int -> Int
pipeBare number =
    number |> negate


prefixArithmetic : Int -> Int -> Int
prefixArithmetic left right =
    (+) left right


composeLeft : Int -> Int
composeLeft number =
    (negate << arity2 3) number


composeRight : Int -> Int
composeRight number =
    (arity2 3 >> negate) number


arity2 : Int -> Int -> Int
arity2 first second =
    first + second


arity3 : Int -> Int -> Int -> Int
arity3 a b c =
    a + b + c


arity4 : Int -> Int -> Int -> Int -> Int
arity4 a b c d =
    a + b + c + d


arity5 : Int -> Int -> Int -> Int -> Int -> Int
arity5 a b c d e =
    a + b + c + d + e


arity6 : Int -> Int -> Int -> Int -> Int -> Int -> Int
arity6 a b c d e f =
    a + b + c + d + e + f


arity7 : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
arity7 a b c d e f g =
    a + b + c + d + e + f + g


arity8 : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
arity8 a b c d e f g h =
    a + b + c + d + e + f + g + h


arity9 : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
arity9 a b c d e f g h i =
    a + b + c + d + e + f + g + h + i


arity10 : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
arity10 a b c d e f g h i j =
    a + b + c + d + e + f + g + h + i + j


partialCall : Int -> Int
partialCall number =
    let
        addThree =
            arity2 3
    in
    addThree number


partialCoreCall : Int -> Int
partialCoreCall number =
    let
        masked =
            Bitwise.and 7
    in
    masked number


partialBasicsCall : Int -> Int
partialBasicsCall number =
    let
        plusThree =
            (+) 3
    in
    plusThree number


caseFanout : Int -> String
caseFanout number =
    case number of
        0 ->
            "zero"

        1 ->
            "one"

        2 ->
            "two"

        _ ->
            "other"


type Wrapped
    = Wrapped Int


unwrap : Wrapped -> Int
unwrap (Wrapped value) =
    value


wrappedAsValue : Int -> Int
wrappedAsValue value =
    List.map Wrapped [ value ] |> List.map unwrap |> List.sum


type Color
    = Red
    | Green
    | Blue


enumPattern : Color -> Int
enumPattern color =
    case color of
        Red ->
            1

        Green ->
            2

        Blue ->
            3


decisionShared : ( Color, Color ) -> Int
decisionShared pair =
    case pair of
        ( Red, Blue ) ->
            1

        ( Green, Red ) ->
            2

        _ ->
            0


type Normal
    = Empty
    | Pair Int Int


normalPattern : Normal -> Int
normalPattern value =
    case value of
        Empty ->
            0

        Pair left right ->
            left + right


recordSpecializations : { longFieldName : Int, otherFieldName : Int } -> Int
recordSpecializations original =
    let
        updated =
            { original | longFieldName = original.longFieldName + 1 }
    in
    .longFieldName updated + updated.otherFieldName


characterPattern : Char -> Int
characterPattern character =
    case character of
        'a' ->
            1

        _ ->
            0


booleanPattern : Bool -> Int
booleanPattern value =
    case value of
        True ->
            1

        False ->
            0


stringPattern : String -> Int
stringPattern value =
    case value of
        "" ->
            0

        "match" ->
            1

        _ ->
            2


unitPattern : () -> Int
unitPattern () =
    1


conditionalExpression : Int -> Int
conditionalExpression number =
    if number < 0 then
        -1

    else if number == 0 then
        0

    else
        1


conditionalBlock : Int -> Int
conditionalBlock number =
    if number < 0 then
        -1

    else if number == 0 then
        0

    else
        let
            result =
                number + 1
        in
        result


literalRepresentations : () -> ( (), Char )
literalRepresentations () =
    ( (), 'x' )


emptyList : () -> List Int
emptyList () =
    []
