module Url.Parser.Query exposing
    ( Parser
    , custom
    , enum
    , int
    , map
    , map2
    , map3
    , map4
    , map5
    , map6
    , map7
    , map8
    , string
    )

import Dict
import Url.Parser.Internal as Q


type QueryParser a
    = Parser (Dict.Dict String (List String) -> a)


type alias Parser a =
    Q.QueryParser a


string : String -> Parser (Maybe String)
string key =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    Just str

                _ ->
                    Nothing


int : String -> Parser (Maybe Int)
int key =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    String.toInt str

                _ ->
                    Nothing


enum : String -> Dict.Dict String a -> Parser (Maybe a)
enum key dict =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    Dict.get str dict

                _ ->
                    Nothing


custom : String -> (List String -> a) -> Parser a
custom key func =
    Q.Parser <|
        \dict ->
            func (Maybe.withDefault [] (Dict.get key dict))


map : (a -> b) -> Parser a -> Parser b
map func (Q.Parser a) =
    Q.Parser <| \dict -> func (a dict)


map2 : (a -> b -> result) -> Parser a -> Parser b -> Parser result
map2 func (Q.Parser a) (Q.Parser b) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict)


map3 : (a -> b -> c -> result) -> Parser a -> Parser b -> Parser c -> Parser result
map3 func (Q.Parser a) (Q.Parser b) (Q.Parser c) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict)


map4 : (a -> b -> c -> d -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser result
map4 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict)


map5 : (a -> b -> c -> d -> e -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser result
map5 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) (Q.Parser e) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict) (e dict)


map6 : (a -> b -> c -> d -> e -> f -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser result
map6 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) (Q.Parser e) (Q.Parser f) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict) (e dict) (f dict)


map7 : (a -> b -> c -> d -> e -> f -> g -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser result
map7 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) (Q.Parser e) (Q.Parser f) (Q.Parser g) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict) (e dict) (f dict) (g dict)


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser result
map8 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) (Q.Parser e) (Q.Parser f) (Q.Parser g) (Q.Parser h) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict) (e dict) (f dict) (g dict) (h dict)
