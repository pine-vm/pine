module Frontend.Page.News exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Font
import Frontend.Visuals as Visuals
import List.Extra


type alias NewsItem =
    { year : Int
    , title : String
    , url : String
    }


newsItems : List NewsItem
newsItems =
    [ { year = 2024
      , title = "Introducing the Pine VS Code extension and Elm language server"
      , url = "https://michaelrätzel.com/blog/introducing-the-pine-vs-code-extension-and-elm-language-server"
      }
    , { year = 2023
      , title = "Database Functions in Elm-Time - Easy Database Updates in Production"
      , url = "https://michaelrätzel.com/blog/database-functions-in-elm-time-easy-database-updates-in-production"
      }
    , { year = 2023
      , title = "Elm Silent Teacher - An Interactive Way to Learn Elm"
      , url = "https://michaelrätzel.com/blog/elm-silent-teacher-an-interactive-way-to-learn-elm"
      }
    , { year = 2021
      , title = "Introducing Elm Editor - a web-based IDE for Elm programs"
      , url = "https://michaelrätzel.com/blog/introducing-elm-editor-a-web-based-ide-for-elm-programs"
      }
    , { year = 2020
      , title = "Design Report - Migrations in Elm Fullstack Deployments"
      , url = "https://michaelrätzel.com/blog/design-report-migrations-in-elm-fullstack-deployments"
      }
    ]


view : Element.Device -> Element.Element e
view _ =
    let
        newsItemElement newsItem =
            Element.link
                [ Element.pointer
                , Element.width Element.fill
                , Element.Font.color Visuals.defaultLinkConfig.color
                , Element.mouseOver
                    [ Element.Font.color (Element.rgb 1 1 1)
                    , Element.Background.color linkHoverColor
                    ]
                , Element.Border.rounded 2
                ]
                { url = newsItem.url
                , label =
                    [ Element.text newsItem.title ]
                        |> Element.paragraph
                            [ Element.padding (Visuals.defaultFontSize // 2) ]
                }
                |> Element.el [ Element.width Element.fill ]
    in
    newsItems
        |> List.sortBy (.year >> negate)
        |> List.Extra.gatherEqualsBy .year
        |> List.map
            (\( yearFirstItem, yearOtherItems ) ->
                [ headingElementFromLevel 3 (String.fromInt yearFirstItem.year)
                , yearFirstItem
                    :: yearOtherItems
                    |> List.map newsItemElement
                    |> Element.column
                        [ Element.spacing (Visuals.defaultFontSize // 2)
                        ]
                ]
                    |> Element.column
                        [ Element.spacing Visuals.defaultFontSize
                        , Element.width Element.fill
                        ]
            )
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing (Visuals.defaultFontSize * 2)
            ]


linkHoverColor : Element.Color
linkHoverColor =
    Element.rgb255 130 180 229


headingElementFromLevel : Int -> String -> Element.Element e
headingElementFromLevel headingLevel =
    Element.text
        >> List.singleton
        >> Element.paragraph (Visuals.headingAttributes headingLevel)
