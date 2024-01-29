module Frontend.Visuals exposing (..)

import Html


globalCssStyleHtmlElement : Html.Html a
globalCssStyleHtmlElement =
    cssStyleHtmlElement globalCssStyleText


cssStyleHtmlElement : String -> Html.Html a
cssStyleHtmlElement style =
    [ Html.text style ]
        |> Html.node "style" []


globalCssStyleText : String
globalCssStyleText =
    [ composeMarkdownCssStyleText { containerClassName = "markdown-body" }

    -- , globalStyleInDedicatedElement
    ]
        |> String.join "\n\n"


composeMarkdownCssStyleText : { containerClassName : String } -> String
composeMarkdownCssStyleText { containerClassName } =
    [ ( [ "h1" ]
      , """
padding-top: 0;
padding-bottom: 0.3em;
font-size: 2em;
border-bottom: 1px solid rgba(255,255,255,0.1);
margin-top: 24px;
margin-bottom: 16px;
font-weight: 600;
line-height: 1.25;
"""
      )
    , ( [ "h2" ]
      , """
padding-top: 0;
padding-bottom: 0.3em;
font-size: 1.5em;
border-bottom: 1px solid rgba(255,255,255,0.1);
margin-top: 24px;
margin-bottom: 16px;
font-weight: 600;
line-height: 1.25;
"""
      )
    , ( [ "h3" ]
      , """
padding-top: 0;
padding-bottom: 0.3em;
font-size: 1.25em;
margin-bottom: 16px;
font-weight: 600;
line-height: 1.25;
"""
      )
    , ( [ "a:link", "a:visited" ]
      , """
color: rgb(77,179,230);
text-decoration: none;
"""
      )
    , ( [ "a:hover", "a:active" ]
      , """
color: rgb(77,179,230);
text-decoration: underline;
"""
      )
    , ( [ "pre" ]
      , """
background-color: rgba(66,111,111,0.3);
margin-top: 0;
padding: 0.8em;
font-size: 90%;
line-height: 1.5;
border-radius: 0.4em;
overflow-y: auto;
"""
      )
    , ( [ "code:not(pre code)" ]
      , """
background-color: rgba(66,111,111,0.4);
padding-block: 0.2em;
padding-inline: 0.4em;
font-size: 90%;
line-height: 1.5;
border-radius: 0.4em;
"""
      )
    , ( [ "p:not(li p)" ]
      , """
margin-top: 0;
"""
      )
    , ( [ "ul" ]
      , """
margin-top: 0;
"""
      )
    ]
        |> List.concatMap
            (\( htmlTags, cssContent ) ->
                htmlTags
                    |> List.map
                        (\htmlTag ->
                            [ "." ++ containerClassName ++ " " ++ htmlTag ++ " {"
                            , String.trim cssContent
                            , "}"
                            ]
                                |> String.join "\n"
                        )
            )
        |> String.join "\n"
