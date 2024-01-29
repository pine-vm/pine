module Frontend.MarkdownElmUI exposing (..)

{-| Based on the example from <https://github.com/dillonkearns/elm-markdown/blob/1b0b3dc4f1170c4e8b00c68a38c49199fd8b067f/examples/src/ElmUi.elm>
-}

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Region
import Html
import Html.Attributes
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


type alias RenderConfig =
    { link :
        { opensInNewTab : Bool
        , color : Element.Color
        , mouseOverColor : Element.Color
        }
    , code : { color : Element.Color }
    }


type alias LinkConfig =
    { opensInNewTab : Bool
    , color : Element.Color
    , mouseOverColor : Element.Color
    }


view : RenderConfig -> String -> Result String (List (Element.Element event))
view renderConfig markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (List.map Markdown.Parser.deadEndToString >> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render (renderer renderConfig))


renderer : RenderConfig -> Markdown.Renderer.Renderer (Element.Element event)
renderer config =
    { heading = heading
    , paragraph = Element.paragraph [ Element.spacing 5 ]
    , thematicBreak = Element.none
    , text = \value -> Element.paragraph [] [ Element.text value ]
    , strong = \content -> Element.paragraph [ Element.Font.bold ] content
    , emphasis = \content -> Element.paragraph [ Element.Font.italic ] content
    , strikethrough = \content -> Element.paragraph [ Element.Font.strike ] content
    , codeSpan = code config
    , link =
        \{ title, destination } body ->
            linkElementFromUrlAndLabel
                { url = destination
                , labelElement =
                    Element.paragraph
                        [ Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
                        , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
                        ]
                        body
                , newTabLink = config.link.opensInNewTab
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.paragraph
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 10 ]
                (items
                    |> List.map
                        (\(Markdown.Block.ListItem task children) ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph
                                    [ Element.alignTop ]
                                    ((case task of
                                        Markdown.Block.IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        Markdown.Block.CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        Markdown.Block.NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 10 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock config
    , table = Element.column []
    , tableHeader =
        Element.column
            [ Element.Font.bold
            , Element.width Element.fill
            , Element.Font.center
            ]
    , tableBody = Element.column []
    , tableRow = Element.row [ Element.height Element.fill, Element.width Element.fill ]
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , tableCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , html = Markdown.Html.oneOf []
    }


alternateTableRowBackground =
    Element.rgb255 245 247 249


tableBorder =
    [ Element.Border.color (Element.rgb255 223 226 229)
    , Element.Border.width 1
    , Element.Border.solid
    , Element.paddingXY 6 13
    , Element.height Element.fill
    ]


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


heading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Element.Element event) } -> Element.Element event
heading { level, rawText, children } =
    Element.paragraph
        [ Element.Font.size
            (case level of
                Markdown.Block.H1 ->
                    25

                Markdown.Block.H2 ->
                    22

                _ ->
                    19
            )
        , Element.Font.bold
        , Element.Region.heading (Markdown.Block.headingLevelToInt level)
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


code : RenderConfig -> String -> Element.Element event
code config snippet =
    Element.el
        [ Element.Background.color config.code.color
        , Element.Border.rounded 2
        , Element.paddingXY 5 2
        , Element.Font.family [ Element.Font.typeface "Consolas", Element.Font.monospace ]
        ]
        (Element.text snippet)


codeBlock : RenderConfig -> { body : String, language : Maybe String } -> Element.Element event
codeBlock config details =
    [ details.body
        |> Html.text
        |> Element.html
        |> Element.el
            [ Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
            , Element.htmlAttribute (Html.Attributes.style "line-height" "normal")

            -- https://github.com/mdgriffith/elm-ui/issues/321#issuecomment-1207450594
            , Element.scrollbarX
            , Element.width Element.fill
            ]
    ]
        |> Element.row
            [ Element.Background.color config.code.color
            , Element.padding 15
            , Element.Font.family [ Element.Font.typeface "Consolas", Element.Font.monospace ]
            , Element.width Element.fill
            ]


linkElementFromUrlAndLabel :
    { url : String, labelElement : Element.Element event, newTabLink : Bool }
    -> Element.Element event
linkElementFromUrlAndLabel { url, labelElement, newTabLink } =
    (if newTabLink then
        Element.newTabLink

     else
        Element.link
    )
        elementLinkStyleAttributes
        { url = url
        , label = labelElement
        }


elementLinkStyleAttributes : List (Element.Attribute a)
elementLinkStyleAttributes =
    [ Element.pointer

    -- https://github.com/mdgriffith/elm-ui/issues/158#issuecomment-624231895
    , Element.Border.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
    , Element.Border.color <| Element.rgba 0 0 0 0
    , Element.mouseOver
        [ Element.Font.color defaultLinkConfig.mouseOverColor
        , Element.Border.color <| defaultLinkConfig.mouseOverColor
        ]
    , Element.Font.color defaultLinkConfig.color
    ]


defaultLinkConfig : LinkConfig
defaultLinkConfig =
    { opensInNewTab = True
    , color = Element.rgb 0.3 0.7 0.9
    , mouseOverColor = Element.rgb 0.3 0.7 0.9
    }
