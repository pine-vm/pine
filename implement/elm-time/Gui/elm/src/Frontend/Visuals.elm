module Frontend.Visuals exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Region
import Html.Attributes as HA


type alias LinkConfig =
    { opensInNewTab : Bool
    , color : Element.Color
    , mouseOverColor : Element.Color
    }


buttonElement :
    List (Element.Attribute event)
    ->
        { label : Element.Element event
        , onPress : Maybe event
        , disabled : Bool
        }
    -> Element.Element event
buttonElement attributes buttonConfig =
    let
        colors =
            primaryButtonBackgroundColor
    in
    Element.Input.button
        ([ Element.Background.color colors.default
         , Element.Font.color (Element.rgb 1 1 1)
         , Element.paddingXY defaultFontSize (defaultFontSize // 2 - 1)
         ]
            ++ attributes
            ++ (if buttonConfig.disabled then
                    [ Element.alpha 0.5
                    , Element.htmlAttribute (HA.disabled True)
                    , Element.htmlAttribute (HA.style "cursor" "default")
                    ]

                else
                    [ Element.Background.color colors.default
                    , Element.mouseOver [ Element.Background.color colors.mouseOver ]
                    ]
               )
        )
        { label = buttonConfig.label
        , onPress = buttonConfig.onPress
        }


primaryButtonBackgroundColor :
    { default : Element.Color
    , mouseOver : Element.Color
    }
primaryButtonBackgroundColor =
    { default = Element.rgb255 0 95 184
    , mouseOver = Element.rgb255 24 110 190
    }


linkElementFromHref : { newTabLink : Bool } -> String -> Element.Element event
linkElementFromHref { newTabLink } href =
    linkElementFromUrlAndTextLabel { url = href, labelText = href, newTabLink = newTabLink }


linkElementFromUrlAndTextLabel :
    { url : String, labelText : String, newTabLink : Bool }
    -> Element.Element event
linkElementFromUrlAndTextLabel { url, labelText, newTabLink } =
    linkElementFromUrlAndLabel
        { url = url
        , labelElement = labelText |> Element.text
        , newTabLink = newTabLink
        }


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
    { opensInNewTab = False
    , color = Element.rgb255 0 0 238
    , mouseOverColor = Element.rgb255 0 0 238
    }


headingAttributes : Int -> List (Element.Attribute event)
headingAttributes rank =
    let
        fontSizePercent =
            max 0 (90 - rank * 20) + 100
    in
    [ elementFontSizePercent fontSizePercent
    , Element.Region.heading rank
    ]


elementFontSizePercent : Int -> Element.Attribute a
elementFontSizePercent percent =
    Element.htmlAttribute (HA.style "font-size" ((percent |> String.fromInt) ++ "%"))


defaultFontSize : Int
defaultFontSize =
    16


errorColor : Element.Color
errorColor =
    Element.rgb 0.8 0.1 0


successColor : Element.Color
successColor =
    Element.rgb 0.1 0.8 0
