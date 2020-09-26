module Backend.Route exposing
    ( Route(..)
    , StaticFile(..)
    , fromAuth0LoginRedirectPath
    , routeFromUrl
    )

import Url
import Url.Parser


type Route
    = ApiRoute
    | StaticFileRoute StaticFile


type StaticFile
    = FrontendElmJavascriptRoute
    | MonacoFrameDocumentRoute
    | MonarchJavascriptRoute


fromAuth0LoginRedirectPath : String
fromAuth0LoginRedirectPath =
    "/login/fromauth0"


routeFromUrl : Url.Url -> Maybe Route
routeFromUrl =
    Url.Parser.parse
        (Url.Parser.oneOf
            [ Url.Parser.map (StaticFileRoute FrontendElmJavascriptRoute) (Url.Parser.s "elm-made.js")
            , Url.Parser.map (StaticFileRoute MonacoFrameDocumentRoute) (Url.Parser.s "monaco")
            , Url.Parser.map (StaticFileRoute MonarchJavascriptRoute) (Url.Parser.s "monarch.js")
            , Url.Parser.map ApiRoute (Url.Parser.s "api")
            ]
        )
