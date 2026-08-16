module Frontend.Page.Home exposing (..)

import Element
import Element.Font
import Frontend.View as View
import Frontend.Visuals as Visuals


view : Element.Element e
view =
    [ "Grow What’s Next"
        |> headingElementFromLevel 2
    , "Build web services, full-stack web apps, games, and command-line tools in Elm. Pine runs Elm natively on .NET across Linux, Windows, and macOS."
        |> View.paragraphFromText
        |> Element.el
            [ Element.Font.bold
            ]
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
