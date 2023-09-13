module Frontend.Page.Download exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Font
import FontAwesome
import FontAwesome.Brands
import Frontend.View as View
import Frontend.Visuals as Visuals


type alias DownloadLinksByPlatform =
    { windows : String
    , linux : String
    , macOS : String
    }


type alias DownloadPlatform =
    { title : String
    , icon : FontAwesome.Icon FontAwesome.WithoutId
    , getDownloadLink : DownloadLinksByPlatform -> String
    }


downloadPlatforms : List DownloadPlatform
downloadPlatforms =
    [ { title = "Linux"
      , icon = FontAwesome.Brands.linux
      , getDownloadLink = .linux
      }
    , { title = "Windows"
      , icon = FontAwesome.Brands.windows
      , getDownloadLink = .windows
      }
    , { title = "macOS"
      , icon = FontAwesome.Brands.apple
      , getDownloadLink = .macOS
      }
    ]


dockerImageUrl : String
dockerImageUrl =
    "https://github.com/elm-time/elm-time/pkgs/container/elm-time"


downloads : DownloadLinksByPlatform
downloads =
    { linux = "https://github.com/elm-time/elm-time/releases/download/v2023-09-12/elm-time-bin-v2023-09-12-linux-x64.zip"
    , windows = "https://github.com/elm-time/elm-time/releases/download/v2023-09-12/elm-time-bin-v2023-09-12-win-x64.zip"
    , macOS = "https://github.com/elm-time/elm-time/releases/download/v2023-09-12/elm-time-bin-v2023-09-12-osx-x64.zip"
    }


view : Element.Device -> Element.Element e
view device =
    [ [ Element.text "Download the pre-built Elm-Time binaries for your platform, and start developing today." ]
        |> Element.paragraph
            (Element.width Element.fill
                :: Visuals.headingAttributes 3
            )
    , downloadPlatforms
        |> List.map viewPlatform
        |> View.responsiveRowOrColumn device
            [ Element.width Element.fill
            , Element.spacing (Visuals.defaultFontSize // 2)
            ]
    , [ [ Element.text "Docker Image "
        , FontAwesome.Brands.docker |> FontAwesome.view |> Element.html
        ]
            |> Element.paragraph (Visuals.headingAttributes 2)
      , [ Element.text "Official Elm-Time Docker Image"
        , Element.text dockerImageUrl
        ]
            |> List.map
                (\linkLabel ->
                    Visuals.linkElementFromUrlAndLabel
                        { url = dockerImageUrl
                        , labelElement = Element.paragraph [] [ linkLabel ]
                        , newTabLink = False
                        }
                )
            |> Element.column
                [ Element.spacing (Visuals.defaultFontSize // 2)
                , Element.width Element.fill
                ]
      ]
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing (Visuals.defaultFontSize * 2)
            ]
    ]
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing (Visuals.defaultFontSize * 2)
            ]


viewPlatform : DownloadPlatform -> Element.Element e
viewPlatform platform =
    let
        downloadUrl =
            platform.getDownloadLink downloads

        downloadFileName =
            downloadUrl
                |> String.split "/"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault downloadUrl
    in
    Element.link
        [ Element.pointer
        , Element.width Element.fill
        , Element.Font.color Visuals.defaultLinkConfig.color
        , Element.mouseOver
            [ Element.Font.color (Element.rgb 1 1 1)
            , Element.Background.color linkHoverColor
            ]
        , Element.Border.rounded 4
        ]
        { url = downloadUrl
        , label =
            [ platform.icon
                |> FontAwesome.view
                |> Element.html
                |> Element.el [ Element.Font.size (Visuals.defaultFontSize * 4), Element.alignTop, Element.centerX ]
            , Element.text platform.title
                |> Element.el (Element.centerX :: Visuals.headingAttributes 3)
            , [ Element.text downloadFileName ]
                |> Element.paragraph [ Element.Font.center ]
                |> Element.el [ Visuals.elementFontSizePercent 90, Element.width Element.fill ]
            ]
                |> Element.column
                    [ Element.width Element.fill
                    , Element.spacing (Visuals.defaultFontSize // 2)
                    , Element.padding Visuals.defaultFontSize
                    ]
        }
        {-
           Avoid bug in layout of 'link' element:
           Apply the workaround shared by Luca at https://github.com/mdgriffith/elm-ui/issues/226#issue-627821005
        -}
        |> Element.el [ Element.width Element.fill ]


linkHoverColor : Element.Color
linkHoverColor =
    Element.rgb255 130 180 229
