module Frontend.View exposing (..)

import Element
import Frontend.Visuals as Visuals
import Html


responsiveRowOrColumn :
    Element.Device
    -> List (Element.Attribute e)
    -> List (Element.Element e)
    -> Element.Element e
responsiveRowOrColumn device attributes elements =
    let
        asRow =
            Element.row attributes elements

        asColumn =
            Element.column attributes elements
    in
    case device.class of
        Element.Phone ->
            asColumn

        Element.Tablet ->
            asRow

        Element.Desktop ->
            asRow

        Element.BigDesktop ->
            asRow


paragraphFromTextKeepingLineBreaks : String -> Element.Element e
paragraphFromTextKeepingLineBreaks text =
    text
        |> String.split "\n"
        |> List.map Element.text
        |> List.intersperse (Element.html (Html.br [] []))
        |> paragraphFromElements


paragraphFromText : String -> Element.Element e
paragraphFromText text =
    paragraphFromElements [ Element.text text ]


paragraphFromElements : List (Element.Element e) -> Element.Element e
paragraphFromElements =
    Element.paragraph Visuals.paragraphAttributesDefault
