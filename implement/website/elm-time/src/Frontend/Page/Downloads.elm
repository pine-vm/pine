module Frontend.Page.Downloads exposing (..)

import Element
import Element.Font
import Frontend.View as View
import Frontend.Visuals as Visuals


type alias DownloadLinksByPlatform =
    { windows : String
    , linux : String
    }


type alias DownloadPlatform =
    { title : String
    , getDownloadLink : DownloadLinksByPlatform -> String
    }


downloadPlatforms : List DownloadPlatform
downloadPlatforms =
    [ { title = "Linux"
      , getDownloadLink = .linux
      }
    , { title = "Windows"
      , getDownloadLink = .windows
      }
    ]


dockerImageUrl : String
dockerImageUrl =
    "https://github.com/elm-time/elm-time/pkgs/container/elm-time"


downloads : DownloadLinksByPlatform
downloads =
    { linux = "https://github.com/elm-time/elm-time/releases/download/v2023-02-16/elm-time-bin-f0d6ce07465ef0ddc4bdbd1f0cef3ab01978f427-linux-x64.zip"
    , windows = "https://github.com/elm-time/elm-time/releases/download/v2023-02-16/elm-time-bin-f0d6ce07465ef0ddc4bdbd1f0cef3ab01978f427-win10-x64.zip"
    }


view : Element.Device -> Element.Element e
view device =
    [ [ Element.text "Download the Elm-Time pre-built binaries for your platform, and start developing today." ]
        |> Element.paragraph
            (Element.width Element.fill
                :: Visuals.headingAttributes 3
            )
    , downloadPlatforms
        |> List.map viewPlatform
        |> View.responsiveRowOrColumn device
            [ Element.width Element.fill
            , Element.spacing Visuals.defaultFontSize
            , Element.padding (Visuals.defaultFontSize * 2)
            ]
    , [ [ Element.text "Docker Image" ]
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
        ]
        { url = downloadUrl
        , label =
            [ Element.text platform.title
                |> Element.el (Element.centerX :: Visuals.headingAttributes 3)
            , [ Element.text downloadFileName ]
                |> Element.paragraph [ Element.centerX ]
            ]
                |> Element.column
                    [ Element.width Element.fill
                    , Element.spacing Visuals.defaultFontSize
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
