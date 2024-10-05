module Frontend.Page.Home exposing (..)

import Element
import Element.Font
import Frontend.View as View
import Frontend.Visuals as Visuals


view : Element.Element e
view =
    [ "Run Elm Everywhere"
        |> headingElementFromLevel 2
    , "Pine is a free, open-source, cross-platform Elm runtime environment that lets developers create servers, web apps, and command line tools."
        |> View.paragraphFromText
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
