module Frontend.Page.Home exposing (..)

import Element
import Element.Font
import Frontend.Visuals as Visuals


view : Element.Element e
view =
    [ "Elm-Time is an open-source, cross-platform runtime environment for the Elm programming language."
        |> headingElementFromLevel 2
    ]
        |> Element.column
            [ Element.spacing (Visuals.defaultFontSize * 2)
            , Element.width Element.fill
            ]


headingElementFromLevel : Int -> String -> Element.Element e
headingElementFromLevel headingLevel =
    Element.text
        >> List.singleton
        >> Element.paragraph
            (Element.Font.center
                :: Element.width Element.fill
                :: Visuals.headingAttributes headingLevel
            )
