module Combine exposing
    ( Parser(..)
    , andThen
    , backtrackable
    , between
    , continueWith
    , end
    , fail
    , fromCore
    , ignore
    , keep
    , lazy
    , many
    , many1
    , many1WithEndLocationForLastElement
    , manyWithEndLocationForLastElement
    , map
    , maybe
    , modifyState
    , oneOf
    , parens
    , runParser
    , sepBy
    , sepBy1
    , sepBy1WithoutReverse
    , string
    , succeed
    , while
    , withLocation
    , withState
    )

import Elm.Syntax.Range exposing (Location, Range)
import Parser as Core exposing ((|=))


type Parser state res
    = Parser (state -> Core.Parser ( state, res ))


fromCore : Core.Parser res -> Parser state res
fromCore p =
    Parser
        (\state ->
            Core.succeed (\v -> ( state, v )) |= p
        )


runParser : Parser state res -> state -> String -> Result (List Core.DeadEnd) ( state, res )
runParser (Parser p) st s =
    Core.run (p st) s


lazy : (() -> Parser s a) -> Parser s a
lazy t =
    Parser (\state -> Core.lazy (\() -> (\(Parser t_) -> t_ state) (t ())))


withState : (s -> Parser s a) -> Parser s a
withState f =
    Parser <|
        \state ->
            (\(Parser p) -> p state) (f state)


modifyState : (s -> s) -> Parser s ()
modifyState f =
    Parser <|
        \state -> Core.succeed ( f state, () )


withLocation : (Location -> Parser s a) -> Parser s a
withLocation f =
    Parser <|
        \state ->
            Core.getPosition
                |> Core.andThen
                    (\( row, col ) ->
                        let
                            (Parser p) =
                                f { row = row, column = col }
                        in
                        p state
                    )


map : (a -> b) -> Parser s a -> Parser s b
map f (Parser p) =
    Parser <|
        \state ->
            p state
                |> Core.map (\( s, a ) -> ( s, f a ))


andThen : (a -> Parser s b) -> Parser s a -> Parser s b
andThen f (Parser p) =
    Parser <|
        \state ->
            p state
                |> Core.andThen (\( s, a ) -> (\(Parser x) -> x s) (f a))


keep : Parser s a -> Parser s (a -> b) -> Parser s b
keep (Parser rp) (Parser lp) =
    Parser <|
        \state ->
            lp state
                |> Core.andThen (\( newState, a ) -> Core.map (Tuple.mapSecond a) (rp newState))


fail : String -> Parser s a
fail m =
    Parser <|
        \state ->
            Core.problem m |> Core.map (\x -> ( state, x ))


succeed : a -> Parser s a
succeed res =
    Parser <| \state -> Core.succeed ( state, res )


string : String -> Parser s String
string s =
    Parser <|
        \state ->
            Core.getChompedString (Core.token s)
                |> Core.map (\x -> ( state, x ))


while : (Char -> Bool) -> Parser s String
while pred =
    Parser <|
        \state ->
            Core.getChompedString (Core.chompWhile pred)
                |> Core.map (\x -> ( state, x ))


end : Parser s ()
end =
    Parser <|
        \state ->
            Core.end |> Core.map (\x -> ( state, x ))


backtrackable : Parser s a -> Parser s a
backtrackable (Parser p) =
    Parser <| \state -> Core.backtrackable (p state)


oneOf : List (Parser s a) -> Parser s a
oneOf xs =
    Parser <| \state -> Core.oneOf (List.map (\(Parser x) -> x state) xs)


maybe : Parser s a -> Parser s (Maybe a)
maybe (Parser p) =
    Parser <|
        \state ->
            Core.oneOf
                [ p state |> Core.map (\( c, v ) -> ( c, Just v ))
                , Core.succeed ( state, Nothing )
                ]


many : Parser s a -> Parser s (List a)
many p =
    manyWithoutReverse [] p
        |> map List.reverse


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.
-}
manyWithoutReverse : List a -> Parser s a -> Parser s (List a)
manyWithoutReverse initList (Parser p) =
    let
        helper : ( s, List a ) -> Core.Parser (Core.Step ( s, List a ) ( s, List a ))
        helper (( oldState, items ) as acc) =
            Core.oneOf
                [ p oldState
                    |> Core.map (\( newState, item ) -> Core.Loop ( newState, item :: items ))
                , Core.succeed (Core.Done acc)
                ]
    in
    Parser <|
        \state ->
            Core.loop ( state, initList ) helper


manyWithEndLocationForLastElement : Range -> (a -> Range) -> Parser s a -> Parser s ( Location, List a )
manyWithEndLocationForLastElement defaultRange getRange (Parser p) =
    let
        helper : ( s, List a ) -> Core.Parser (Core.Step ( s, List a ) ( s, ( Location, List a ) ))
        helper ( oldState, items ) =
            Core.oneOf
                [ p oldState
                    |> Core.map (\( newState, item ) -> Core.Loop ( newState, item :: items ))
                , Core.succeed ()
                    |> Core.map
                        (\() ->
                            Core.Done ( oldState, ( endLocationForList defaultRange getRange items, List.reverse items ) )
                        )
                ]
    in
    Parser <|
        \state ->
            Core.loop ( state, [] ) helper


many1WithEndLocationForLastElement : (a -> Range) -> Parser s a -> Parser s ( Location, List a )
many1WithEndLocationForLastElement getRange p =
    p
        |> andThen
            (\a ->
                manyWithEndLocationForLastElement (getRange a) getRange p
                    |> map (\( location, list ) -> ( location, a :: list ))
            )


endLocationForList : Range -> (a -> Range) -> List a -> Location
endLocationForList defaultRange getRange list =
    case list of
        [] ->
            defaultRange.end

        a :: _ ->
            (getRange a).end


many1 : Parser s a -> Parser s (List a)
many1 p =
    succeed (::)
        |> keep p
        |> keep (many p)


sepBy : Parser s x -> Parser s a -> Parser s (List a)
sepBy sep p =
    oneOf
        [ sepBy1 sep p
        , succeed []
        ]


sepBy1 : Parser s x -> Parser s a -> Parser s (List a)
sepBy1 sep p =
    succeed (::)
        |> keep p
        |> keep (many (sep |> continueWith p))


{-| Same as [`sepBy1`](#sepBy1), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.
-}
sepBy1WithoutReverse : Parser s x -> Parser s a -> Parser s (List a)
sepBy1WithoutReverse sep p =
    p
        |> andThen (\first -> manyWithoutReverse [ first ] (sep |> continueWith p))


between : Parser s l -> Parser s r -> Parser s a -> Parser s a
between lp rp p =
    lp
        |> continueWith p
        |> ignore rp


parens : Parser s a -> Parser s a
parens =
    between (string "(") (string ")")


ignore : Parser s x -> Parser s a -> Parser s a
ignore dropped target =
    target
        |> map always
        |> keep dropped


continueWith : Parser s a -> Parser s x -> Parser s a
continueWith target dropped =
    dropped
        |> map (\_ a -> a)
        |> keep target
