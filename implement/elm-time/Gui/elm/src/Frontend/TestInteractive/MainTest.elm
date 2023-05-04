module Frontend.TestInteractive.MainTest exposing (..)

import Frontend.BrowserApplicationInitWithTime
import Frontend.Main exposing (..)


main : Frontend.BrowserApplicationInitWithTime.Program () State Event
main =
    Frontend.Main.mainWithCustomizedInitAndUpdate
        (\normalInit url key time ->
            let
                ( ( state, customCmd ), cmd ) =
                    time
                        |> normalInit url key
                        |> Tuple.mapFirst init
            in
            ( state, [ customCmd ] |> Cmd.batch )
        )
        (\normalUpdate event ->
            normalUpdate event
        )


init : State -> ( State, Cmd.Cmd Event )
init defaultState =
    let
        stateBefore =
            { defaultState
                | adminInterfaceConfig =
                    Just
                        (Ok
                            { elmTimeVersionId = "2044-13-44"
                            , httpRoutes =
                                [ { path = "/demo/route/path"
                                  , methods = [ "post", "get" ]
                                  }
                                ]
                            }
                        )
            }
    in
    ( stateBefore
    , Cmd.none
    )
