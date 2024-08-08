module Frontend.Page.Home exposing (..)

import Element
import Element.Font
import Frontend.View as View
import Frontend.Visuals as Visuals


view : Element.Element e
view =
    [ "Run Elm Everywhere"
        |> headingElementFromLevel 2
    , "Pine is a runtime for the Elm programming language. It's built on .NET and runs on Linux, Windows, and macOS."
        |> View.paragraphFromText
    , "Pine supports building and running Elm programs natively on .NET without JavaScript."
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
