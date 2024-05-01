module Backend.Route exposing
    ( Route(..)
    , StaticFile(..)
    , elmMadeScriptFileNameDebug
    , elmMadeScriptFileNameDefault
    , fromAuth0LoginRedirectPath
    , routeFromUrl
    )

import Url
import Url.Parser


type Route
    = ApiRoute
    | StaticFileRoute StaticFile


type StaticFile
    = FrontendHtmlDocumentRoute { debug : Bool }
    | FrontendElmJavascriptRoute { debug : Bool }
    | FrontendLanguageServiceWorkerJavaScriptRoute


elmMadeScriptFileNameDefault : String
elmMadeScriptFileNameDefault =
    "elm-made.js"


elmMadeScriptFileNameDebug : String
elmMadeScriptFileNameDebug =
    "elm-made-debug.js"


fromAuth0LoginRedirectPath : String
fromAuth0LoginRedirectPath =
    "/login/fromauth0"


languageServiceWorkerJavascriptPath : String
languageServiceWorkerJavascriptPath =
    "language-service-worker.js"


routeFromUrl : Url.Url -> Maybe Route
routeFromUrl =
    Url.Parser.parse
        (Url.Parser.oneOf
            [ Url.Parser.map (StaticFileRoute (FrontendElmJavascriptRoute { debug = False })) (Url.Parser.s elmMadeScriptFileNameDefault)
            , Url.Parser.map (StaticFileRoute (FrontendElmJavascriptRoute { debug = True })) (Url.Parser.s elmMadeScriptFileNameDebug)
            , Url.Parser.map (StaticFileRoute FrontendLanguageServiceWorkerJavaScriptRoute) (Url.Parser.s languageServiceWorkerJavascriptPath)
            , Url.Parser.map ApiRoute (Url.Parser.s "api")
            , Url.Parser.map (StaticFileRoute (FrontendHtmlDocumentRoute { debug = True })) (Url.Parser.s "enable-elm-debug")
            ]
        )
