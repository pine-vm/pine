module Frontend.Page.Home exposing (..)

import Element
import Element.Font
import Frontend.View as View
import Frontend.Visuals as Visuals


view : Element.Element e
view =
    [ "Elm-Time is an open-source, cross-platform runtime environment for the Elm programming language."
        |> headingElementFromLevel 2
    , "Elm-Time integrates a web server and a database management system, automating the persistence and maintenance of application state and database migrations."
        |> View.paragraphFromText
    , "The Elm-Time compiler offers various interfaces supporting the automatic generation of Elm code at build time. This automation frees applications from boilerplate and glue code and allows us to focus on business logic."
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
