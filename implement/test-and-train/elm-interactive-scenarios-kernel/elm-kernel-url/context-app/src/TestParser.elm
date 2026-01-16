module TestParser exposing (..)

import Url
import Url.Parser exposing ((</>))


type Route
    = EntryRoute (Maybe Int)


routeFromUrl : Url.Url -> Maybe Route
routeFromUrl =
    Url.Parser.parse
        (Url.Parser.oneOf
            [ Url.Parser.map (Just >> EntryRoute) (Url.Parser.s "api" </> Url.Parser.s "entry" </> Url.Parser.int)
            , Url.Parser.map (EntryRoute Nothing) (Url.Parser.s "api" </> Url.Parser.s "entry")
            ]
        )
