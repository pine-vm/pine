module Coverage exposing (results)

import CoverageTypes as Types exposing (Choice(..), Person)


results : List String
results =
    List.concat
        [ literals
        , collections
        , operators
        , applications
        , records
        , bindings
        , branches
        , List.map tuplePatterns
            [ ( 1, ( 2, 3 ), ( "four", True, () ) )
            , ( -1, ( 0, 7 ), ( "", False, () ) )
            ]
        , List.map recordPatterns
            [ Person "Ada" 36 True
            , { name = "", age = 0, active = False }
            ]
        , List.map namedPatterns
            [ Empty
            , Single 7
            , Pair 8 "pair"
            , Nested Nothing
            , Nested (Just ( 9, [ 10, 11 ] ))
            , Triple 12 True "triple"
            ]
        , List.map listPatterns
            [ []
            , [ 1 ]
            , [ 2, 3 ]
            , [ 4, 5, 6 ]
            , [ 7, 8, 9, 10 ]
            ]
        , List.map aliasPatterns
            [ ( Just ( 1, 2 ), [ 3, 4 ] )
            , ( Nothing, [] )
            ]
        , List.map literalPatterns
            [ ( (), ( 'a', "exact", 1 ) )
            , ( (), ( '\n', "", 0 ) )
            , ( (), ( '🙂', "other", 0x2A ) )
            , ( (), ( 'z', "fallback", 99 ) )
            ]
        , List.map booleanPatterns [ True, False ]
        , List.map nestedPatterns
            [ [ ( Just (Types.Envelope ( 1, { label = "nested", values = [ 2, 3 ] } )), Ok ( 4, 5 ) ) ]
            , [ ( Nothing, Err "failed" ) ]
            , []
            ]
        , List.map envelopeArgument
            [ Types.Envelope ( 14, { label = "argument", values = [ 15, 16 ] } )
            , Types.Envelope ( 0, { label = "", values = [] } )
            ]
        ]


literals : List String
literals =
    [ Debug.toString ()
    , "quotes: \"double\", 'single', slash: \\, tab:\tnewline:\n"
    , """first line
second line with "quotes" and a backslash: \\
Unicode: \u{03BB} and \u{1F642}"""
    , String.fromChar 'x'
    , String.fromChar '\n'
    , String.fromChar '🙂'
    , String.fromInt 0
    , String.fromInt 123456
    , String.fromInt 0x2A
    , String.fromInt -17
    , String.fromFloat 0.125
    , String.fromFloat 6.25e2
    , String.fromFloat -(2.5 + 0.5)
    ]


collections : List String
collections =
    [ Debug.toString []
    , Debug.toString [ 1, 2, 3 ]
    , Debug.toString [ [], [ 1 ], [ 2, 3 ] ]
    , Debug.toString ( (), 7 )
    , Debug.toString ( 1, "two", True )
    , Debug.toString ( ( 1, 2 ), ( 3, ( 4, 5 ) ) )
    , Debug.toString {}
    , Debug.toString { z = ( 1, 2 ), a = [ 3, 4 ], nested = { flag = True } }
    , Debug.toString [ Empty, Single 1, Pair 2 "three", Nested (Just ( 4, [] )) ]
    ]


operators : List String
operators =
    [ String.fromInt (2 + 3 * 4 - 5)
    , String.fromFloat (7 / 2)
    , String.fromInt (17 // 3)
    , String.fromInt (2 ^ 5)
    , Debug.toString (3 == 3 && 4 /= 5)
    , Debug.toString (1 < 2 && 2 <= 2 && 3 > 2 && 3 >= 3)
    , Debug.toString (False || (True && not False))
    , String.join "," ("a" :: [ "b", "c" ])
    , "left" ++ "-" ++ "right"
    , Debug.toString ([ 1, 2 ] ++ [ 3 ] ++ [])
    , String.fromInt ((+) 6 7)
    , Debug.toString ((::) 8 [ 9 ])
    , String.fromInt ((\n -> n + 1) <| 10)
    , 11 |> (\n -> n * 2) |> String.fromInt
    , String.fromInt (((\n -> n + 1) << (\n -> n * 2)) 12)
    , String.fromInt (((\n -> n + 1) >> (\n -> n * 2)) 13)
    ]


applications : List String
applications =
    let
        add =
            (+) 10

        makePair left right =
            ( left, right )

        choose yes no flag =
            if flag then
                yes

            else
                no

        combine a b c d e f g h i =
            a + b + c + d + e + f + g + h + i
    in
    [ String.fromInt (add 5)
    , Debug.toString (makePair "left" (add 6))
    , choose "yes" "no" True
    , String.fromInt (combine 1 2 3 4 5 6 7 8 9)
    , Debug.toString (List.map (\( left, right ) -> left + right) [ ( 1, 2 ), ( 3, 4 ) ])
    , Debug.toString (List.map (\{ age } -> age + 1) [ Person "Grace" 40 True ])
    , (\() -> "unit lambda") ()
    , (\_ -> "wildcard lambda") 99
    , List.map
        (\(( left, right ) as pair) -> Debug.toString ( pair, left + right ))
        [ ( 1, 2 ), ( 3, 4 ) ]
        |> String.join ","
    , List.map
        (\(Types.Envelope ( number, { label } )) -> label ++ String.fromInt number)
        [ Types.Envelope ( 5, { label = "lambda", values = [] } ) ]
        |> String.join ","
    , List.foldl (\value total -> value + total) 0 [ 1, 2, 3 ] |> String.fromInt
    ]


records : List String
records =
    let
        original =
            Person "Ada" 36 True

        updated =
            { original | age = original.age + 1, name = original.name ++ "!" }

        nested =
            { person = updated, extra = { score = 7 } }
    in
    [ original.name
    , String.fromInt updated.age
    , nested.person.name
    , String.fromInt nested.extra.score
    , .name original
    , List.map .age [ original, updated ] |> Debug.toString
    , Debug.toString updated
    , Debug.toString (updateRecord { value = 2, preserved = "keep" })
    ]


updateRecord : { record | value : Int } -> { record | value : Int }
updateRecord record =
    { record | value = record.value * 3 }


bindings : List String
bindings =
    let
        after =
            before + 1

        before =
            20

        ( first, ( second, third ) ) =
            ( 1, ( 2, 3 ) )

        ( fourth, fifth, sixth ) =
            ( 4, 5, 6 )

        { name, age } =
            Person "Local" 7 True

        (Types.Envelope ( wrapped, { label, values } )) =
            Types.Envelope ( 8, { label = "wrapped", values = [ 9, 10 ] } )

        sumTo remaining total =
            if remaining <= 0 then
                total

            else
                sumTo (remaining - 1) (total + remaining)

        nonTail remaining =
            if remaining <= 0 then
                0

            else
                1 + nonTail (remaining - 1)

        localResult =
            let
                twice value =
                    value * 2
            in
            twice after
    in
    [ Debug.toString ( first, second, third )
    , Debug.toString ( fourth, fifth, sixth )
    , name ++ String.fromInt age
    , label ++ Debug.toString ( wrapped, values )
    , String.fromInt (sumTo 4 0)
    , String.fromInt (nonTail 3)
    , String.fromInt localResult
    ]


branches : List String
branches =
    [ conditional -1
    , conditional 0
    , conditional 2
    , case chooseIdentity 3 of
        ( result, flag ) ->
            Debug.toString ( result, flag )
    , (case List.head [ 1, 2 ] of
        Just value ->
            value + 1

        Nothing ->
            0
      )
        |> String.fromInt
    ]


conditional : Int -> String
conditional value =
    if value < 0 then
        "negative"

    else if value == 0 then
        "zero"

    else
        let
            sign =
                if value > 1 then
                    "large"

                else
                    "small"
        in
        sign ++ String.fromInt value


chooseIdentity : Int -> ( Int, Bool )
chooseIdentity value =
    (\item -> ( item, item > 0 )) value


tuplePatterns : ( Int, ( Int, Int ), ( String, Bool, () ) ) -> String
tuplePatterns ( first, ( second, third ), ( label, flag, () ) ) =
    Debug.toString ( first + second + third, label, flag )


recordPatterns : Person -> String
recordPatterns ({ name, age, active } as person) =
    name ++ Debug.toString ( age, active, person )


namedPatterns : Choice -> String
namedPatterns choice =
    case choice of
        Empty ->
            "empty"

        Single value ->
            String.fromInt value

        Pair number text ->
            text ++ String.fromInt number

        Nested (Just ( number, head :: tail )) ->
            Debug.toString ( number, head, tail )

        Nested (Just ( number, [] )) ->
            "nested empty" ++ String.fromInt number

        Nested Nothing ->
            "nothing"

        Triple number flag text ->
            Debug.toString ( number, flag, text )


listPatterns : List Int -> String
listPatterns items =
    case items of
        [] ->
            "nil"

        [ one ] ->
            "singleton" ++ String.fromInt one

        [ first, second ] ->
            Debug.toString ( first, second )

        first :: second :: rest ->
            Debug.toString ( first, second, rest )


aliasPatterns : ( Maybe ( Int, Int ), List Int ) -> String
aliasPatterns input =
    case input of
        ( Just (( left, right ) as pair), (head :: tail) as whole ) as all ->
            Debug.toString ( pair, whole, ( left + right + head, tail, all ) )

        ( _, [] ) ->
            "empty"

        ( Nothing, _ ) ->
            "no pair"


literalPatterns : ( (), ( Char, String, Int ) ) -> String
literalPatterns ( (), ( character, text, number ) ) =
    let
        charResult =
            case character of
                'a' ->
                    "letter"

                '\n' ->
                    "newline"

                '🙂' ->
                    "unicode"

                _ ->
                    "other char"

        stringResult =
            case text of
                "" ->
                    "empty string"

                "exact" ->
                    "matched string"

                _ ->
                    "other string"

        intResult =
            case number of
                1 ->
                    "one"

                0 ->
                    "zero"

                0x2A ->
                    "hex"

                _ ->
                    "other int"
    in
    String.join ":" [ charResult, stringResult, intResult ]


booleanPatterns : Bool -> String
booleanPatterns flag =
    case flag of
        True ->
            "true"

        False ->
            "false"


nestedPatterns : List ( Maybe Types.Envelope, Result String ( Int, Int ) ) -> String
nestedPatterns items =
    case items of
        ( Just (Types.Envelope ( number, { label, values } )), Ok ( left, right ) ) :: rest ->
            Debug.toString ( number + left + right, ( label, values ), rest )

        ( Nothing, Err message ) :: _ ->
            message

        _ ->
            "fallback"


envelopeArgument : Types.Envelope -> String
envelopeArgument ((Types.Envelope ( number, { label, values } )) as envelope) =
    Debug.toString ( envelope, number, ( label, values ) )
