module Frontend.Visuals exposing (..)

import Dict
import Element
import Html
import Svg
import Svg.Attributes


type Icon
    = ChatActionIcon
    | GitHubActionIcon
    | GrowActionIcon
    | ShrinkActionIcon
    | DirectoryExpandedIcon
    | DirectoryCollapsedIcon
    | CloseEditorIcon
    | FileTypeElmIcon
    | FileTypeJsonIcon
    | FileTypeMarkdownIcon


iconFromFileName : String -> Maybe ( Icon, String )
iconFromFileName fileName =
    let
        ending =
            fileName
                |> String.split "."
                |> List.reverse
                |> List.head
                |> Maybe.withDefault fileName
                |> String.toLower
    in
    if ending == fileName then
        Nothing

    else
        fileTypeIconFromFileNameEnding |> Dict.get ending


fileTypeIconFromFileNameEnding : Dict.Dict String ( Icon, String )
fileTypeIconFromFileNameEnding =
    [ ( "elm", ( FileTypeElmIcon, "#529BBA" ) )
    , ( "json", ( FileTypeJsonIcon, "#DBCD68" ) )
    , ( "md", ( FileTypeMarkdownIcon, "#529BBA" ) )
    ]
        |> Dict.fromList


iconSvgElementFromIcon : { color : String } -> Icon -> Element.Element event
iconSvgElementFromIcon { color } iconType =
    let
        iconSvg =
            iconSvgPathsData iconType

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
        , Svg.Attributes.fill color
        ]
        pathsElements
        |> Element.html


iconSvgPathsData : Icon -> { width : Int, height : Int, offsetX : Int, offsetY : Int, paths : List { pathData : String, fillNone : Bool } }
iconSvgPathsData icon =
    case icon of
        ChatActionIcon ->
            -- https://github.com/google/material-design-icons/tree/96206ade0e8325ac4c4ce9d49dc4ef85241689e1/src/communication/chat_bubble
            { width = 24
            , height = 24
            , offsetX = 0
            , offsetY = 0
            , paths =
                [ { pathData = "M20 2H4c-1.1 0-2 .9-2 2v18l4-4h14c1.1 0 2-.9 2-2V4c0-1.1-.9-2-2-2z"
                  , fillNone = False
                  }
                ]
            }

        GitHubActionIcon ->
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

        GrowActionIcon ->
            { width = 24
            , height = 24
            , offsetX = 0
            , offsetY = 0
            , paths =
                [ { pathData = "M14.5 0L14.5 2.5L19.5 2.5L13.5 8.5L15.5 10.5L21.0 5.0L21.0 9.5L24.0 9.5L24.0 0L14.5 0Z"
                  , fillNone = False
                  }
                , { pathData = "M9.5 24.0L9.5 21.5L4.5 21.5L10.5 15.5L8.5 13.5L3.0 19.0L3.0 14.5L0 14.5L0 24.0L9.5 24.0Z"
                  , fillNone = False
                  }
                ]
            }

        ShrinkActionIcon ->
            { width = 24
            , height = 24
            , offsetX = 0
            , offsetY = 0
            , paths =
                [ { pathData = "M23.0 10.5L23.0 8.0L18.0 8.0L24.0 2.0L22.0 0L16.5 5.5L16.5 1.0L13.5 1.0L13.5 10.5L23.0 10.5Z"
                  , fillNone = False
                  }
                , { pathData = "M1.0 13.5L1.0 16.0L6.0 16.0L0 22.0L2.0 24.0L7.5 18.5L7.5 23.0L10.5 23.0L10.5 13.5L1.0 13.5Z"
                  , fillNone = False
                  }
                ]
            }

        DirectoryExpandedIcon ->
            { width = 200
            , height = 200
            , offsetX = 0
            , offsetY = 0
            , paths =
                [ { pathData = "M20 40L100 120L180 40L200 60L100 160L0 60L20 40Z"
                  , fillNone = False
                  }
                ]
            }

        DirectoryCollapsedIcon ->
            { width = 200
            , height = 200
            , offsetX = 0
            , offsetY = 0
            , paths =
                [ { pathData = "M40 20L120 100L40 180L60 200L160 100L60 0L40 20Z"
                  , fillNone = False
                  }
                ]
            }

        CloseEditorIcon ->
            { width = 200
            , height = 200
            , offsetX = 0
            , offsetY = 0
            , paths =
                [ { pathData = "M0 20L80 100L0 180L20 200L100 120L180 200L200 180L120 100L200 20L180 0L100 80L20 0L0 20Z"
                  , fillNone = False
                  }
                ]
            }

        FileTypeElmIcon ->
            { width = 640
            , height = 640
            , offsetX = 0
            , offsetY = 0
            , paths =
                [ { pathData = "M0 620L300 320L0 20L0 620Z"
                  , fillNone = False
                  }
                , { pathData = "M620 640L20 640L320 338.92L620 640Z"
                  , fillNone = False
                  }
                , { pathData = "M160 140L460 140L320 0L20 0L160 140Z"
                  , fillNone = False
                  }
                , { pathData = "M640 280L640 0L360 0L640 280Z"
                  , fillNone = False
                  }
                , { pathData = "M320 300L460 160L180 160L320 300Z"
                  , fillNone = False
                  }
                , { pathData = "M490 470L640 320L490 168.92L340 320L490 470Z"
                  , fillNone = False
                  }
                , { pathData = "M640 620L640 360L510 490L640 620Z"
                  , fillNone = False
                  }
                ]
            }

        FileTypeJsonIcon ->
            -- https://github.com/jesseweed/seti-ui/blob/d6f6730b464f6bbacd641f1a915af570cdc69ed8/icons/json.svg
            { width = 18
            , height = 18
            , offsetX = 7
            , offsetY = 7
            , paths =
                [ { pathData = "M7.5 15.1c1.5 0 1.7-.8 1.7-1.5 0-.6-.1-1.1-.1-1.7S9 10.7 9 10.2c0-2.1 1.3-3 3.4-3h.8v1.9h-.4c-1 0-1.3.6-1.3 1.6 0 .4.1.8.1 1.3 0 .4.1.9.1 1.5 0 1.7-.7 2.3-1.9 2.6 1.2.3 1.9.9 1.9 2.6 0 .6-.1 1.1-.1 1.5 0 .4-.1.9-.1 1.2 0 1 .3 1.6 1.3 1.6h.4v1.9h-.8c-2 0-3.3-.8-3.3-3 0-.6 0-1.1.1-1.7.1-.6.1-1.2.1-1.7 0-.6-.2-1.5-1.7-1.5l-.1-1.9zm17 1.7c-1.5 0-1.7.9-1.7 1.5s.1 1.1.1 1.7c.1.6.1 1.2.1 1.7 0 2.2-1.4 3-3.4 3h-.8V23h.4c1 0 1.3-.6 1.3-1.6 0-.4 0-.8-.1-1.2 0-.5-.1-1-.1-1.5 0-1.7.7-2.3 1.9-2.6-1.2-.3-1.9-.9-1.9-2.6 0-.6.1-1.1.1-1.5.1-.5.1-.9.1-1.3 0-1-.4-1.5-1.3-1.6h-.4V7.2h.8c2.1 0 3.4.9 3.4 3 0 .6-.1 1.1-.1 1.7-.1.6-.1 1.2-.1 1.7 0 .7.2 1.5 1.7 1.5v1.7z"
                  , fillNone = False
                  }
                ]
            }

        FileTypeMarkdownIcon ->
            -- https://github.com/jesseweed/seti-ui/blob/d6f6730b464f6bbacd641f1a915af570cdc69ed8/icons/markdown.svg
            { width = 20
            , height = 20
            , offsetX = 6
            , offsetY = 6
            , paths =
                [ { pathData = "M20.7 6.7v9.9h3.8c-2.9 3-5.8 5.9-8.7 8.8-2.7-2.8-5.6-5.8-8.4-8.7h3.5V6.6c1.3.9 4.4 3.1 5 3.1.6 0 3.6-2.2 4.8-3z"
                  , fillNone = False
                  }
                ]
            }


elmEditorIconSvg : String -> Html.Html e
elmEditorIconSvg size =
    let
        foregroundPaths =
            [ "M 42.901 68.565 L 42.901 76.396 L 11 76.396 L 11 20 L 42.901 20 L 42.901 27.792 L 20.219 27.792 L 20.219 43.145 L 41.474 43.145 L 41.474 50.859 L 20.219 50.859 L 20.219 68.565 L 42.901 68.565 Z"
            , "M 89.344 52.209 L 89.344 57.108 L 60.915 57.108 Q 61.108 63.319 64.271 66.656 A 11.689 11.689 0 0 0 73.182 69.992 Q 76.962 69.992 80.221 69.279 A 36.901 36.901 0 0 0 87.223 66.906 L 87.223 74.274 A 27.832 27.832 0 0 1 80.511 76.511 A 41.157 41.157 0 0 1 72.757 77.167 A 20.638 20.638 0 0 1 57.193 71.362 Q 51.58 65.556 51.58 55.373 A 24.326 24.326 0 0 1 56.626 39.128 A 18.314 18.314 0 0 1 56.788 38.94 A 17.987 17.987 0 0 1 71.099 32.961 A 17.58 17.58 0 0 1 84.445 38.091 A 19.604 19.604 0 0 1 89.344 52.209 Z M 61.069 50.551 L 80.434 50.551 A 11.809 11.809 0 0 0 77.888 42.547 A 8.708 8.708 0 0 0 71.099 39.827 Q 66.778 39.827 64.175 42.566 Q 61.571 45.305 61.069 50.551 Z"
            , "M0 100L0 0L100 0L100 100L0 100ZM4 97L97 97L97 3L3 3L3 97Z"
            ]

        backgroundPathData =
            "M1 99L1 1L99 1L99 99L1 99Z"

        foregroundPathsElements =
            foregroundPaths
                |> List.map
                    (\pathData -> Svg.path [ Svg.Attributes.d pathData, Svg.Attributes.fill "#7BCA39" ] [])
    in
    Svg.svg
        [ Svg.Attributes.viewBox ([ 0, 0, 100, 100 ] |> List.map String.fromInt |> String.join " ")
        , Svg.Attributes.width size
        , Svg.Attributes.height size
        ]
        (Svg.path [ Svg.Attributes.d backgroundPathData, Svg.Attributes.fill "#1A2B0C" ] []
            :: foregroundPathsElements
        )
