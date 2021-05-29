module Backend.Route exposing
    ( Route(..)
    , StaticFile(..)
    , elmMadeScriptFileNameDebug
    , elmMadeScriptFileNameDefault
    , fromAuth0LoginRedirectPath
    , routeFromUrl
    )

import Common
import Url
import Url.Parser


type Route
    = ApiRoute
    | StaticFileRoute StaticFile


type StaticFile
    = FrontendHtmlDocumentRoute { debug : Bool }
    | FrontendElmJavascriptRoute { debug : Bool }
    | MonacoFrameDocumentRoute
    | MonarchJavascriptRoute
    | FaviconRoute


elmMadeScriptFileNameDefault : String
elmMadeScriptFileNameDefault =
    "elm-made.js"


elmMadeScriptFileNameDebug : String
elmMadeScriptFileNameDebug =
    "elm-made-debug.js"


fromAuth0LoginRedirectPath : String
fromAuth0LoginRedirectPath =
    "/login/fromauth0"


routeFromUrl : Url.Url -> Maybe Route
routeFromUrl =
    Url.Parser.parse
        (Url.Parser.oneOf
            [ Url.Parser.map (StaticFileRoute (FrontendElmJavascriptRoute { debug = False })) (Url.Parser.s elmMadeScriptFileNameDefault)
            , Url.Parser.map (StaticFileRoute (FrontendElmJavascriptRoute { debug = True })) (Url.Parser.s elmMadeScriptFileNameDebug)
            , Url.Parser.map (StaticFileRoute MonacoFrameDocumentRoute) (Url.Parser.s "monaco")
            , Url.Parser.map (StaticFileRoute MonarchJavascriptRoute) (Url.Parser.s "monarch.js")
            , Url.Parser.map (StaticFileRoute FaviconRoute) (Url.Parser.s Common.faviconPath)
            , Url.Parser.map ApiRoute (Url.Parser.s "api")
            , Url.Parser.map (StaticFileRoute (FrontendHtmlDocumentRoute { debug = True })) (Url.Parser.s "enable-elm-debug")
            ]
        )
