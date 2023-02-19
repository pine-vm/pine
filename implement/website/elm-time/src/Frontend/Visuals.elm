module Frontend.Visuals exposing (..)

import Element
import Element.Border
import Element.Font
import Element.Region
import Html
import Html.Attributes
import Svg
import Svg.Attributes


type alias LinkConfig =
    { opensInNewTab : Bool
    , color : Element.Color
    , mouseOverColor : Element.Color
    }


type alias SvgIcon =
    { width : Int
    , height : Int
    , offsetX : Int
    , offsetY : Int
    , paths : List { pathData : String, fillNone : Bool }
    }


iconSvgElementFromIcon : { color : String, width : String, height : String } -> SvgIcon -> Element.Element event
iconSvgElementFromIcon config iconSvg =
    let
        pathsElements =
            iconSvg.paths
                |> List.map
                    (\pathInfo ->
                        let
                            fillAttributes =
                                if pathInfo.fillNone then
                                    [ Svg.Attributes.fill "none" ]

                                else
                                    []
                        in
                        Svg.path (Svg.Attributes.d pathInfo.pathData :: fillAttributes) []
                    )
    in
    Svg.svg
        [ Svg.Attributes.viewBox
            ([ iconSvg.offsetX
             , iconSvg.offsetY
             , iconSvg.width
             , iconSvg.height
             ]
                |> List.map String.fromInt
                |> String.join " "
            )
        , Svg.Attributes.fill config.color
        , Html.Attributes.style "width" config.width
        , Html.Attributes.style "height" config.height
        ]
        pathsElements
        |> Element.html


gitHubIcon : SvgIcon
gitHubIcon =
    -- https://github.com/microsoft/vscode-codicons/tree/e1155a851abafe070be17d36996474f4f374741f/src/icons
    { width = 24
    , height = 24
    , offsetX = 0
    , offsetY = 0
    , paths =
        [ { pathData = "M12 1a11 11 0 1 0 0 22 11 11 0 0 0 0-22zm2.9 19.968h-.086a.471.471 0 0 1-.35-.129.471.471 0 0 1-.129-.34v-1.29c.006-.428.01-.86.01-1.297a3.385 3.385 0 0 0-.139-.943 1.679 1.679 0 0 0-.496-.802 7.34 7.34 0 0 0 1.868-.432 3.715 3.715 0 0 0 1.344-.883c.373-.392.65-.864.81-1.381.196-.632.289-1.29.276-1.952a3.797 3.797 0 0 0-.24-1.353 3.569 3.569 0 0 0-.727-1.177c.068-.172.118-.351.148-.534a3.286 3.286 0 0 0-.036-1.262 4.87 4.87 0 0 0-.203-.7.269.269 0 0 0-.102-.018h-.1c-.21.002-.419.037-.618.102-.22.064-.436.144-.645.239a5.97 5.97 0 0 0-.606.314 9.992 9.992 0 0 0-.525.332 8.78 8.78 0 0 0-4.714 0 12.367 12.367 0 0 0-.525-.332 5.52 5.52 0 0 0-.616-.314 4.14 4.14 0 0 0-.646-.239 2.02 2.02 0 0 0-.607-.102h-.1a.266.266 0 0 0-.1.019 5.356 5.356 0 0 0-.213.699 3.441 3.441 0 0 0-.027 1.262c.03.183.079.362.147.534a3.565 3.565 0 0 0-.726 1.177 3.797 3.797 0 0 0-.24 1.353 6.298 6.298 0 0 0 .266 1.942c.167.517.443.992.811 1.391.38.386.838.687 1.344.883.598.23 1.225.377 1.863.437-.178.161-.32.36-.414.58-.09.219-.153.448-.184.682a2.524 2.524 0 0 1-1.077.248 1.639 1.639 0 0 1-.976-.276 2.661 2.661 0 0 1-.69-.755 2.914 2.914 0 0 0-.267-.35 2.459 2.459 0 0 0-.34-.314 1.687 1.687 0 0 0-.397-.22 1.1 1.1 0 0 0-.441-.093.942.942 0 0 0-.11.01c-.05 0-.1.006-.148.018a.376.376 0 0 0-.12.055.107.107 0 0 0-.054.091.304.304 0 0 0 .129.222c.084.068.155.12.212.157l.026.019c.123.094.24.196.35.305.104.09.197.192.276.303.083.108.154.226.212.349.067.123.138.264.212.424.172.434.478.802.874 1.05.415.223.882.334 1.353.322.16 0 .32-.01.48-.028.156-.025.313-.052.47-.083v1.598a.459.459 0 0 1-.488.477h-.057a9.428 9.428 0 1 1 5.797 0v.005z"
          , fillNone = False
          }
        ]
    }


headingAttributes : Int -> List (Element.Attribute event)
headingAttributes rank =
    let
        fontSizePercent =
            max 0 (190 - rank * 50) + 100
    in
    [ elementFontSizePercent fontSizePercent
    , Element.Region.heading rank
    , Element.htmlAttribute (Html.Attributes.style "letter-spacing" "0.05em")
    , Element.Font.semiBold
    , Element.htmlAttribute htmlAttributeLineHeightNormal
    ]


emailLinkFromAddress : String -> Element.Element e
emailLinkFromAddress emailAddress =
    Element.link elementLinkStyleAttributes
        { url = "mailto:" ++ emailAddress
        , label = Element.text emailAddress
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
    , Element.Font.color defaultLinkConfig.color

    -- https://github.com/mdgriffith/elm-ui/issues/158#issuecomment-624231895
    , Element.Border.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
    , Element.Border.color <| Element.rgba 0 0 0 0
    , Element.mouseOver
        [ Element.Font.color defaultLinkConfig.mouseOverColor
        , Element.Border.color <| defaultLinkConfig.mouseOverColor
        ]
    ]


defaultLinkConfig : LinkConfig
defaultLinkConfig =
    { opensInNewTab = True
    , color = Element.rgb255 9 105 218
    , mouseOverColor = Element.rgb255 9 105 218
    }


paragraphAttributesDefault : List (Element.Attribute e)
paragraphAttributesDefault =
    [ Element.htmlAttribute (Html.Attributes.style "line-height" "1.6em")
    ]


htmlAttributeLineHeightNormal : Html.Attribute a
htmlAttributeLineHeightNormal =
    Html.Attributes.style "line-height" "normal"


elementFontSizePercent : Int -> Element.Attribute a
elementFontSizePercent percent =
    Element.htmlAttribute (Html.Attributes.style "font-size" ((percent |> String.fromInt) ++ "%"))


globalCssStyleHtmlElement : Html.Html a
globalCssStyleHtmlElement =
    cssStyleHtmlElement globalCssStyleText


cssStyleHtmlElement : String -> Html.Html a
cssStyleHtmlElement style =
    [ Html.text style ]
        |> Html.node "style" []


globalCssStyleText : String
globalCssStyleText =
    [ composeMarkdownCssStyleText { containerClassName = "markdown-body" }
    ]
        |> String.join "\n\n"


composeMarkdownCssStyleText : { containerClassName : String } -> String
composeMarkdownCssStyleText { containerClassName } =
    [ ( [ "h1" ]
      , """
padding-top: 0;
padding-bottom: 0.3em;
font-size: 2em;
border-bottom: 1px solid rgba(255,255,255,0.1);
margin-top: 24px;
margin-bottom: 16px;
font-weight: 600;
line-height: 1.25;
"""
      )
    , ( [ "h2" ]
      , """
padding-top: 0;
padding-bottom: 0.3em;
font-size: 1.5em;
border-bottom: 1px solid rgba(255,255,255,0.1);
margin-top: 24px;
margin-bottom: 16px;
font-weight: 600;
line-height: 1.25;
"""
      )
    , ( [ "h3" ]
      , """
padding-top: 0;
padding-bottom: 0.3em;
font-size: 1.25em;
margin-bottom: 16px;
font-weight: 600;
line-height: 1.25;
"""
      )
    , ( [ "a:link", "a:visited" ]
      , """
color: rgb(9, 105, 218);
text-decoration: none;
"""
      )
    , ( [ "a:hover", "a:active" ]
      , """
color: rgb(9, 105, 218);
text-decoration: underline;
"""
      )
    , ( [ "pre" ]
      , """
background-color: rgba(66,111,111,0.3);
margin-top: 0;
padding: 0.8em;
font-size: 90%;
line-height: 1.5;
border-radius: 0.4em;
overflow-y: auto;
"""
      )
    , ( [ "code:not(pre code)" ]
      , """
background-color: rgba(66,111,111,0.4);
padding-block: 0.2em;
padding-inline: 0.4em;
font-size: 90%;
line-height: 1.5;
border-radius: 0.4em;
"""
      )
    , ( [ "p:not(li p)" ]
      , """
margin-top: 0;
"""
      )
    , ( [ "ul" ]
      , """
margin-top: 0;
"""
      )
    ]
        |> List.concatMap
            (\( htmlTags, cssContent ) ->
                htmlTags
                    |> List.map
                        (\htmlTag ->
                            [ "." ++ containerClassName ++ " " ++ htmlTag ++ " {"
                            , String.trim cssContent
                            , "}"
                            ]
                                |> String.join "\n"
                        )
            )
        |> String.join "\n"


rootFontFamily : List String
rootFontFamily =
    [ "Segoe UI", "Tahoma", "Geneva", "Verdana", "sans-serif" ]


defaultFontSize : Int
defaultFontSize =
    15


defaultFontColor : Element.Color
defaultFontColor =
    Element.rgb255 36 41 47


backgroundColor : Element.Color
backgroundColor =
    Element.rgb255 255 255 255


headerBackgroundColor : Element.Color
headerBackgroundColor =
    Element.rgb255 36 41 47


footerBackgroundColor : Element.Color
footerBackgroundColor =
    backgroundColor
