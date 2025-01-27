module Url.Parser exposing
    ( (</>)
    , (<?>)
    , Parser
    , custom
    , fragment
    , int
    , map
    , oneOf
    , parse
    , query
    , s
    , string
    , top
    )

import Dict exposing (Dict)
import Url exposing (Url)
import Url.Parser.Internal as Q
import Url.Parser.Query as Query


infix right 7 (</>) = slash
infix left  8 (<?>) = questionMark


type Parser a b
    = Parser (State a -> List (State b))


type alias State value =
    { visited : List String
    , unvisited : List String
    , params : Dict String (List String)
    , frag : Maybe String
    , value : value
    }


string : Parser (String -> a) a
string =
    custom "STRING" Just


int : Parser (Int -> a) a
int =
    custom "NUMBER" String.toInt


s : String -> Parser a a
s str =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            case unvisited of
                [] ->
                    []

                next :: rest ->
                    if next == str then
                        [ State (next :: visited) rest params frag value ]

                    else
                        []


custom : String -> (String -> Maybe a) -> Parser (a -> b) b
custom tipe stringToSomething =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            case unvisited of
                [] ->
                    []

                next :: rest ->
                    case stringToSomething next of
                        Just nextValue ->
                            [ State (next :: visited) rest params frag (value nextValue) ]

                        Nothing ->
                            []


slash : Parser a b -> Parser b c -> Parser a c
slash (Parser parseBefore) (Parser parseAfter) =
    Parser <|
        \state ->
            List.concatMap parseAfter (parseBefore state)


map : a -> Parser a b -> Parser (b -> c) c
map subValue (Parser parseArg) =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            List.map (mapState value) <|
                parseArg <|
                    State visited unvisited params frag subValue


mapState : (a -> b) -> State a -> State b
mapState func { visited, unvisited, params, frag, value } =
    State visited unvisited params frag (func value)


oneOf : List (Parser a b) -> Parser a b
oneOf parsers =
    Parser <|
        \state ->
            List.concatMap (\(Parser parser) -> parser state) parsers


top : Parser a a
top =
    Parser <| \state -> [ state ]


questionMark : Parser a (query -> b) -> Query.Parser query -> Parser a b
questionMark parser queryParser =
    slash parser (query queryParser)


query : Query.Parser query -> Parser (query -> a) a
query (Q.Parser queryParser) =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            [ State visited unvisited params frag (value (queryParser params))
            ]


fragment : (Maybe String -> fragment) -> Parser (fragment -> a) a
fragment toFrag =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            [ State visited unvisited params frag (value (toFrag frag))
            ]


parse : Parser (a -> a) a -> Url -> Maybe a
parse (Parser parser) url =
    getFirstMatch <|
        parser <|
            State [] (preparePath url.path) (prepareQuery url.query) url.fragment identity


getFirstMatch : List (State a) -> Maybe a
getFirstMatch states =
    case states of
        [] ->
            Nothing

        state :: rest ->
            case state.unvisited of
                [] ->
                    Just state.value

                [ "" ] ->
                    Just state.value

                _ ->
                    getFirstMatch rest


preparePath : String -> List String
preparePath path =
    case String.split "/" path of
        "" :: segments ->
            removeFinalEmpty segments

        segments ->
            removeFinalEmpty segments


removeFinalEmpty : List String -> List String
removeFinalEmpty segments =
    case segments of
        [] ->
            []

        "" :: [] ->
            []

        segment :: rest ->
            segment :: removeFinalEmpty rest


prepareQuery : Maybe String -> Dict String (List String)
prepareQuery maybeQuery =
    case maybeQuery of
        Nothing ->
            Dict.empty

        Just qry ->
            List.foldr addParam Dict.empty (String.split "&" qry)


addParam : String -> Dict String (List String) -> Dict String (List String)
addParam segment dict =
    case String.split "=" segment of
        [ rawKey, rawValue ] ->
            case Url.percentDecode rawKey of
                Nothing ->
                    dict

                Just key ->
                    case Url.percentDecode rawValue of
                        Nothing ->
                            dict

                        Just value ->
                            Dict.update key (addToParametersHelp value) dict

        _ ->
            dict


addToParametersHelp : a -> Maybe (List a) -> Maybe (List a)
addToParametersHelp value maybeList =
    case maybeList of
        Nothing ->
            Just [ value ]

        Just list ->
            Just (value :: list)
