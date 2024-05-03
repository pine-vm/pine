module Frontend.FileEditor exposing (..)


type FileContentType
    = ElmContent
    | JsonContent
    | XmlContent
    | MarkdownContent
    | HtmlContent
    | CssContent


fileContentTypeFromFileName : String -> Maybe FileContentType
fileContentTypeFromFileName fileName =
    case fileName |> String.split "." |> List.reverse of
        [] ->
            Nothing

        fileExtension :: _ ->
            case String.toLower fileExtension of
                "elm" ->
                    Just ElmContent

                "json" ->
                    Just JsonContent

                "xml" ->
                    Just XmlContent

                "md" ->
                    Just MarkdownContent

                "html" ->
                    Just HtmlContent

                "css" ->
                    Just CssContent

                _ ->
                    Nothing
