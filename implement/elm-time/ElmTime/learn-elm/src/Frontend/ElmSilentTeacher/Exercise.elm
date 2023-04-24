module Frontend.ElmSilentTeacher.Exercise exposing (..)

import Random
import Random.Char
import Random.Extra
import Random.List
import Random.String
import String.Extra


type alias Exercise =
    { challengeGenerator : Random.Generator ExerciseChallenge }


type alias ExerciseChallenge =
    String


exercises : List Exercise
exercises =
    [ ( integerAddition, 4 )
    , ( integerSubtraction, 4 )
    , ( integerMultiplication, 3 )
    , ( integerDivision, 7 )
    , ( concatStrings, 4 )
    , ( concatLists, 5 )
    , ( equals, 4 )
    , ( equalsNot, 5 )
    , ( lessThan, 5 )
    , ( notEquals, 6 )
    , ( greaterThanOrEqual, 5 )
    , ( letBlockSimple, 5 )
    , ( letBlockWithTwoDeclarations, 5 )
    , ( listLength, 3 )
    , ( stringLeft, 4 )
    , ( stringDrop, 4 )
    , ( listTake, 4 )
    , ( listHead, 4 )
    , ( listDrop, 4 )
    , ( ifThenElse, 7 )
    , ( simplestNamedFunction, 5 )
    , ( namedFunctionWithTwoArguments, 5 )
    , ( twoNamedFunctions, 4 )
    , ( pipeline, 5 )
    ]
        |> List.map (\( generator, repetitions ) -> List.repeat repetitions (Exercise generator))
        |> interleaveExercisesRecursive


integerAddition : Random.Generator ExerciseChallenge
integerAddition =
    Random.map2
        (\x y ->
            String.fromInt x ++ " + " ++ String.fromInt y
        )
        (Random.int 0 9)
        (Random.int 0 9)


integerSubtraction : Random.Generator ExerciseChallenge
integerSubtraction =
    Random.map2
        (\x y ->
            String.fromInt x ++ " - " ++ String.fromInt y
        )
        (Random.int 0 9)
        (Random.int 0 9)


integerMultiplication : Random.Generator ExerciseChallenge
integerMultiplication =
    Random.map2
        (\x y ->
            String.fromInt x ++ " * " ++ String.fromInt y
        )
        (Random.uniform 0 [ -4, -2, -1, 1, 2, 4, 6 ])
        (Random.uniform 0 [ -2, -1, 1, 2, 4 ])


integerDivision : Random.Generator ExerciseChallenge
integerDivision =
    Random.map2
        (\x y ->
            String.fromInt x ++ " // " ++ String.fromInt y
        )
        (Random.int -10 14)
        (Random.uniform -3 [ -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8 ])


concatStrings : Random.Generator ExerciseChallenge
concatStrings =
    Random.map2
        (\x y ->
            "\"" ++ x ++ "\" ++ \"" ++ y ++ "\""
        )
        (Random.String.string 2 Random.Char.lowerCaseLatin)
        (Random.String.string 1 Random.Char.lowerCaseLatin)


concatLists : Random.Generator ExerciseChallenge
concatLists =
    Random.map2
        (\x y ->
            listSyntaxFromItemsSyntaxes (List.map String.fromInt x)
                ++ " ++ "
                ++ listSyntaxFromItemsSyntaxes (List.map String.fromInt y)
        )
        (Random.Extra.rangeLengthList 0 2 (Random.int 0 11))
        (Random.Extra.rangeLengthList 0 2 (Random.int 0 11))


letBlockSimple : Random.Generator ExerciseChallenge
letBlockSimple =
    Random.map2
        (\x y ->
            """
let
    a = """ ++ String.fromInt x ++ """
in
    a + """ ++ String.fromInt y
        )
        (Random.int 0 9)
        (Random.int 0 9)


letBlockWithTwoDeclarations : Random.Generator ExerciseChallenge
letBlockWithTwoDeclarations =
    Random.map2
        (\x y ->
            """
let
    a = \"""" ++ x ++ """"
    b = \"""" ++ y ++ """"
in
    a ++ b"""
        )
        (Random.String.string 2 Random.Char.lowerCaseLatin)
        (Random.String.string 1 Random.Char.lowerCaseLatin)


equals : Random.Generator ExerciseChallenge
equals =
    Random.map2
        (\x y ->
            String.fromInt x ++ " == " ++ String.fromInt y
        )
        (Random.uniform 0 [ 3, 4 ])
        (Random.uniform 0 [ 3, 4 ])


equalsNot : Random.Generator ExerciseChallenge
equalsNot =
    Random.map2
        (\x y ->
            String.fromInt x ++ " /= " ++ String.fromInt y
        )
        (Random.uniform 7 [ 3, 4 ])
        (Random.uniform 7 [ 3, 4 ])


lessThan : Random.Generator ExerciseChallenge
lessThan =
    Random.map2
        (\x y ->
            String.fromInt x ++ " < " ++ String.fromInt y
        )
        (Random.int -9 9)
        (Random.int -9 9)


notEquals : Random.Generator ExerciseChallenge
notEquals =
    Random.map3
        (\operator x y ->
            "not ( " ++ String.fromInt x ++ " " ++ operator ++ " " ++ String.fromInt y ++ " )"
        )
        (Random.uniform "==" [ "/=" ])
        (Random.uniform 7 [ 3, 4 ])
        (Random.uniform 7 [ 3, 4 ])


greaterThanOrEqual : Random.Generator ExerciseChallenge
greaterThanOrEqual =
    Random.map2
        (\x y ->
            String.fromInt x ++ " >= " ++ String.fromInt y
        )
        (Random.int -9 9)
        (Random.int -9 9)


listLength : Random.Generator ExerciseChallenge
listLength =
    Random.map2
        (\randomList length ->
            "List.length [ " ++ (String.join ", " (List.take length randomList) ++ " ]")
        )
        (List.range 0 9 |> Random.List.shuffle |> Random.map (List.map String.fromInt))
        (Random.int 0 5)


stringLeft : Random.Generator ExerciseChallenge
stringLeft =
    Random.map2
        (\count string ->
            "String.left " ++ String.fromInt count ++ " \"" ++ string ++ "\""
        )
        (Random.int 0 5)
        (Random.String.rangeLengthString 1 4 Random.Char.lowerCaseLatin)


stringDrop : Random.Generator ExerciseChallenge
stringDrop =
    Random.map3
        (\function count string ->
            "String." ++ function ++ " " ++ String.fromInt count ++ " \"" ++ string ++ "\""
        )
        (Random.uniform "dropLeft" [ "dropRight" ])
        (Random.int 0 3)
        (Random.String.rangeLengthString 1 5 Random.Char.lowerCaseLatin)


listTake : Random.Generator ExerciseChallenge
listTake =
    Random.map2
        (\count list ->
            "List.take " ++ String.fromInt count ++ " " ++ listSyntaxFromItemsSyntaxes (List.map String.fromInt list)
        )
        (Random.int 0 5)
        (Random.Extra.rangeLengthList 0 4 (Random.int 0 9))


listHead : Random.Generator ExerciseChallenge
listHead =
    Random.map2
        (\randomList length ->
            "List.head [ " ++ (String.join ", " (List.take length randomList) ++ " ]")
        )
        (List.range 0 9 |> Random.List.shuffle |> Random.map (List.map String.fromInt))
        (Random.int 0 3)


listDrop : Random.Generator ExerciseChallenge
listDrop =
    Random.map2
        (\randomList dropCount ->
            "List.drop " ++ String.fromInt dropCount ++ " [ " ++ (String.join ", " randomList ++ " ]")
        )
        (List.range 0 9 |> Random.List.shuffle |> Random.map (List.take 3 >> List.map String.fromInt))
        (Random.int 0 4)


ifThenElse : Random.Generator ExerciseChallenge
ifThenElse =
    Random.map5
        (\leftOperand operator rightOperand ifTrue ifFalse ->
            [ "if " ++ String.fromInt leftOperand ++ " " ++ operator ++ " " ++ String.fromInt rightOperand ++ " then"
            , "    " ++ String.fromInt ifTrue
            , "else"
            , "    " ++ String.fromInt ifFalse
            ]
                |> String.join "\n"
        )
        (Random.int -9 9)
        (Random.uniform "==" [ "/=", "<", ">", "<=", ">=" ])
        (Random.int -9 9)
        (Random.int 0 9)
        (Random.int 0 9)


simplestNamedFunction : Random.Generator ExerciseChallenge
simplestNamedFunction =
    Random.map2
        (\x y ->
            """
let
    hello a =
        a + """ ++ String.fromInt x ++ """
in
hello """ ++ String.fromInt y
        )
        (Random.int 0 9)
        (Random.int 0 9)


namedFunctionWithTwoArguments : Random.Generator ExerciseChallenge
namedFunctionWithTwoArguments =
    Random.map2
        (\x y ->
            """
let
    hello a b =
        a + b
in
hello """ ++ String.fromInt y ++ " " ++ String.fromInt x
        )
        (Random.int 0 9)
        (Random.int 0 9)


twoNamedFunctions : Random.Generator ExerciseChallenge
twoNamedFunctions =
    Random.map3
        (\x y z ->
            """
let
    salve a b =
        a + b

    hi a b =
        a * b

    osmose = hi """ ++ String.fromInt y ++ " " ++ String.fromInt x ++ """
in
salve osmose """ ++ String.fromInt z
        )
        (Random.int 0 9)
        (Random.int 0 9)
        (Random.int 0 4)


pipeline : Random.Generator ExerciseChallenge
pipeline =
    [ "pizza", "lasagna", "risotto", "focaccia", "arancino", "tiramisu" ]
        |> Random.List.shuffle
        |> Random.map (List.take 3 >> List.map (String.Extra.surround "\"") >> String.join ", ")
        |> Random.andThen
            (\list ->
                Random.int 1 3
                    |> Random.map (\dropCount -> "[ " ++ list ++ """ ]
|> List.drop """ ++ String.fromInt dropCount ++ """
|> List.head
""")
            )


listSyntaxFromItemsSyntaxes : List String -> String
listSyntaxFromItemsSyntaxes items =
    "[ " ++ String.join ", " items ++ " ]"


interleaveExercisesRecursive : List (List a) -> List a
interleaveExercisesRecursive =
    List.map chuckExercisesBeforeInterleaving
        >> interleaveListsRecursive
        >> List.concat


chuckExercisesBeforeInterleaving : List a -> List (List a)
chuckExercisesBeforeInterleaving list =
    case list of
        first :: second :: others ->
            [ first, second ] :: List.map List.singleton others

        _ ->
            List.map List.singleton list


interleaveListsRecursive : List (List a) -> List a
interleaveListsRecursive =
    let
        interleaveSingleListInFront front following =
            case front of
                [] ->
                    following

                next :: frontRemaining ->
                    interleaveSingleListInFront
                        frontRemaining
                        (insertInListAtIndexOrBound (List.length frontRemaining - 1) next following)
    in
    List.map List.reverse
        >> List.foldr interleaveSingleListInFront []


insertInListAtIndexOrBound : Int -> a -> List a -> List a
insertInListAtIndexOrBound i item list =
    List.take i list ++ [ item ] ++ List.drop i list
