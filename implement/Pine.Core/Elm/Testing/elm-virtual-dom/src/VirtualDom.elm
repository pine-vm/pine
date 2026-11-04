module VirtualDom exposing
    ( Node
    , text
    , node
    , nodeNS
    , Attribute
    , style
    , property
    , attribute
    , attributeNS
    , on
    , Handler(..)
    , map
    , mapAttribute
    , keyedNode
    , keyedNodeNS
    , lazy
    , lazy2
    , lazy3
    , lazy4
    , lazy5
    , lazy6
    , lazy7
    , lazy8
    )

{-| A pure virtual DOM for inspecting views in Pine tests, not a browser renderer.
Lazy views are forced only when the test query inspects them.
-}

import Json.Decode as Decode
import Json.Encode as Encode
import VirtualDom.Pine as Internal


type alias Node msg =
    Internal.Node msg


type alias Attribute msg =
    Internal.Attribute msg


type Handler msg
    = Normal (Decode.Decoder msg)
    | MayStopPropagation (Decode.Decoder ( msg, Bool ))
    | MayPreventDefault (Decode.Decoder ( msg, Bool ))
    | Custom (Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool })


text : String -> Node msg
text =
    Internal.Text


node : String -> List (Attribute msg) -> List (Node msg) -> Node msg
node tag =
    Internal.Element Nothing (safeTag tag)


nodeNS : String -> String -> List (Attribute msg) -> List (Node msg) -> Node msg
nodeNS namespace tag =
    Internal.Element (Just namespace) (safeTag tag)


keyedNode : String -> List (Attribute msg) -> List ( String, Node msg ) -> Node msg
keyedNode tag attributes children =
    node tag attributes (List.map Tuple.second children)


keyedNodeNS : String -> String -> List (Attribute msg) -> List ( String, Node msg ) -> Node msg
keyedNodeNS namespace tag attributes children =
    nodeNS namespace tag attributes (List.map Tuple.second children)


style : String -> String -> Attribute msg
style =
    Internal.Style


property : String -> Encode.Value -> Attribute msg
property key value =
    Internal.Property
        (if List.member key [ "innerHTML", "outerHTML", "formAction" ] then
            "data-" ++ key

         else
            key
        )
        (case Decode.decodeValue Decode.string value of
            Ok string ->
                Encode.string (safeUri string)

            Err _ ->
                value
        )


attribute : String -> String -> Attribute msg
attribute key value =
    Internal.Attribute Nothing (safeAttribute key) (safeUri value)


attributeNS : String -> String -> String -> Attribute msg
attributeNS namespace key value =
    Internal.Attribute (Just namespace) (safeAttribute key) (safeUri value)


on : String -> Handler msg -> Attribute msg
on name handler =
    Internal.Event name
        (case handler of
            Normal decoder ->
                Decode.map (\message -> { message = message, stopPropagation = False, preventDefault = False }) decoder

            MayStopPropagation decoder ->
                Decode.map (\( message, stop ) -> { message = message, stopPropagation = stop, preventDefault = False }) decoder

            MayPreventDefault decoder ->
                Decode.map (\( message, prevent ) -> { message = message, stopPropagation = False, preventDefault = prevent }) decoder

            Custom decoder ->
                decoder
        )


map : (a -> b) -> Node a -> Node b
map =
    Internal.mapNode


mapAttribute : (a -> b) -> Attribute a -> Attribute b
mapAttribute =
    Internal.mapAttribute


lazy : (a -> Node msg) -> a -> Node msg
lazy view a =
    Internal.Lazy (\() -> view a)


lazy2 : (a -> b -> Node msg) -> a -> b -> Node msg
lazy2 view a b =
    Internal.Lazy (\() -> view a b)


lazy3 : (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
lazy3 view a b c =
    Internal.Lazy (\() -> view a b c)


lazy4 : (a -> b -> c -> d -> Node msg) -> a -> b -> c -> d -> Node msg
lazy4 view a b c d =
    Internal.Lazy (\() -> view a b c d)


lazy5 : (a -> b -> c -> d -> e -> Node msg) -> a -> b -> c -> d -> e -> Node msg
lazy5 view a b c d e =
    Internal.Lazy (\() -> view a b c d e)


lazy6 : (a -> b -> c -> d -> e -> f -> Node msg) -> a -> b -> c -> d -> e -> f -> Node msg
lazy6 view a b c d e f =
    Internal.Lazy (\() -> view a b c d e f)


lazy7 : (a -> b -> c -> d -> e -> f -> g -> Node msg) -> a -> b -> c -> d -> e -> f -> g -> Node msg
lazy7 view a b c d e f g =
    Internal.Lazy (\() -> view a b c d e f g)


lazy8 : (a -> b -> c -> d -> e -> f -> g -> h -> Node msg) -> a -> b -> c -> d -> e -> f -> g -> h -> Node msg
lazy8 view a b c d e f g h =
    Internal.Lazy (\() -> view a b c d e f g h)


safeTag : String -> String
safeTag tag =
    if String.toLower tag == "script" then
        "p"

    else
        tag


safeAttribute : String -> String
safeAttribute key =
    if String.startsWith "on" (String.toLower key) || String.toLower key == "formaction" then
        "data-" ++ key

    else
        key


safeUri : String -> String
safeUri value =
    let
        normalized =
            value
                |> String.toLower
                |> String.filter (\char -> not (List.member char [ ' ', '\t', '\n', '\u{000D}', '\u{000B}', '\u{000C}', '\u{00A0}', '﻿' ]))
    in
    if String.startsWith "javascript:" normalized || String.startsWith "data:text/html," normalized || String.startsWith "data:text/html;" normalized then
        ""

    else
        value
