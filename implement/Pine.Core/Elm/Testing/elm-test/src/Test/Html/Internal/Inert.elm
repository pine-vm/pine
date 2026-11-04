module Test.Html.Internal.Inert exposing (Node, Error(..), fromElmHtml, fromHtml, parseAttribute, toElmHtml)

import Dict
import Html exposing (Html)
import Json.Decode as Decode
import Test.Html.Internal.ElmHtml.InternalTypes as Types exposing (ElmHtml(..))
import VirtualDom
import VirtualDom.Pine as Dom


type Node msg
    = Node (ElmHtml msg)


type Error
    = DecodeError Decode.Error
    | ValidationErrors { deduped : List Types.Validation }


fromHtml : Html msg -> Result Error (Node msg)
fromHtml html =
    convert html |> Result.map Node


convert : Dom.Node msg -> Result Error (ElmHtml msg)
convert html =
    case html of
        Dom.Text text ->
            Ok (TextTag { text = text })

        Dom.Lazy thunk ->
            convert (thunk ())

        Dom.Element _ tag attributes children ->
            Result.map2
                (\facts converted ->
                    NodeEntry
                        { tag = tag
                        , facts = facts
                        , children = converted
                        , descendantsCount = List.sum (List.map descendants converted)
                        }
                )
                (toFacts attributes)
                (List.foldr (Result.map2 (::) << convert) (Ok []) children)


descendants : ElmHtml msg -> Int
descendants html =
    case html of
        NodeEntry record ->
            1 + record.descendantsCount

        _ ->
            1


toFacts : List (Dom.Attribute msg) -> Result Error (Types.Facts msg)
toFacts attributes =
    let
        facts =
            List.foldl addFact Types.emptyFacts attributes
    in
    if Dict.member "class" facts.stringAttributes && Dict.member "className" facts.stringAttributes then
        Err (ValidationErrors { deduped = [ Types.ClassVsClassNameValidation ] })

    else
        Ok facts


addFact : Dom.Attribute msg -> Types.Facts msg -> Types.Facts msg
addFact attribute facts =
    case attribute of
        Dom.Style key value ->
            { facts | styles = Dict.insert key value facts.styles }

        Dom.Event name decoder ->
            { facts | events = Dict.insert name (VirtualDom.Custom decoder) facts.events }

        Dom.Attribute _ key value ->
            { facts | stringAttributes = insertAttribute "class" key value facts.stringAttributes }

        Dom.Property key value ->
            case Decode.decodeValue Decode.string value of
                Ok string ->
                    { facts | stringAttributes = insertAttribute "className" key string facts.stringAttributes }

                Err _ ->
                    case Decode.decodeValue Decode.bool value of
                        Ok bool ->
                            { facts | boolAttributes = Dict.insert key bool facts.boolAttributes }

                        Err _ ->
                            facts


insertAttribute : String -> String -> String -> Dict.Dict String String -> Dict.Dict String String
insertAttribute classKey key value dictionary =
    if key == classKey then
        Dict.update key (Maybe.map (\old -> old ++ " " ++ value) >> Maybe.withDefault value >> Just) dictionary

    else
        Dict.insert key value dictionary


fromElmHtml : ElmHtml msg -> Node msg
fromElmHtml =
    Node


toElmHtml : Node msg -> ElmHtml msg
toElmHtml (Node html) =
    html


parseAttribute : Html.Attribute msg -> Result String Types.Attribute
parseAttribute attribute =
    case attribute of
        Dom.Style key value ->
            Ok (Types.Style { key = key, value = value })

        Dom.Property key value ->
            Ok (Types.Property { key = key, value = value })

        Dom.Attribute Nothing key value ->
            Ok (Types.Attribute { key = key, value = value })

        Dom.Attribute (Just namespace) key value ->
            Ok (Types.NamespacedAttribute { namespace = namespace, key = key, value = value })

        Dom.Event _ _ ->
            Err "Event handlers cannot be used as attribute selectors."
