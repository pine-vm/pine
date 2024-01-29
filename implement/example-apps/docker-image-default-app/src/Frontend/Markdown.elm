module Frontend.Markdown exposing (..)

import Element
import Html.Attributes
import Markdown.Parser
import Markdown.Renderer


viewViaDefaultHtmlRenderer : { containerClassName : String } -> String -> Result String (List (Element.Element event))
viewViaDefaultHtmlRenderer { containerClassName } markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (List.map Markdown.Parser.deadEndToString >> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
        |> Result.map
            (List.map
                (Element.html
                    >> (List.singleton
                            >> Element.paragraph
                                [ Element.htmlAttribute (Html.Attributes.style "line-height" "1.5")
                                , Element.htmlAttribute (Html.Attributes.class containerClassName)
                                ]
                       )
                )
            )
