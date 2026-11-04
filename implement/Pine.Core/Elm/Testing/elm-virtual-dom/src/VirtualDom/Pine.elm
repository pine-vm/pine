module VirtualDom.Pine exposing (Attribute(..), EventData, Node(..), mapAttribute, mapNode)

import Json.Decode as Decode
import Json.Encode as Encode


type Node msg
    = Text String
    | Element (Maybe String) String (List (Attribute msg)) (List (Node msg))
    | Lazy (() -> Node msg)


type Attribute msg
    = Style String String
    | Property String Encode.Value
    | Attribute (Maybe String) String String
    | Event String (Decode.Decoder (EventData msg))


type alias EventData msg =
    { message : msg, stopPropagation : Bool, preventDefault : Bool }


mapNode : (a -> b) -> Node a -> Node b
mapNode transform node =
    case node of
        Text value ->
            Text value

        Element namespace tag attributes children ->
            Element namespace tag (List.map (mapAttribute transform) attributes) (List.map (mapNode transform) children)

        Lazy thunk ->
            Lazy (\() -> mapNode transform (thunk ()))


mapAttribute : (a -> b) -> Attribute a -> Attribute b
mapAttribute transform attribute =
    case attribute of
        Style key value ->
            Style key value

        Property key value ->
            Property key value

        Attribute namespace key value ->
            Attribute namespace key value

        Event name decoder ->
            Event name
                (Decode.map
                    (\event ->
                        { message = transform event.message
                        , stopPropagation = event.stopPropagation
                        , preventDefault = event.preventDefault
                        }
                    )
                    decoder
                )
