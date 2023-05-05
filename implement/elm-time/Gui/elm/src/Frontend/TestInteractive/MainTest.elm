module Frontend.TestInteractive.MainTest exposing (..)

import Frontend.BrowserApplicationInitWithTime
import Frontend.Main exposing (..)
import HostInterface


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
                    Just (Ok fakeAdminInterfaceConfig)
            }
    in
    ( stateBefore
    , Cmd.none
    )


fakeAdminInterfaceConfig : HostInterface.AdminInterfaceConfig
fakeAdminInterfaceConfig =
    { elmTimeVersionId = "2044-13-44"
    , httpRoutes =
        [ { path = "/demo/route/path"
          , methods = [ "post", "get" ]
          }
        ]
    , functionsApplicableOnDatabase =
        [ { functionName = "Backend.ExposeFunctionsToAdmin.function_update"
          , parameters =
                [ { name = "param_1"
                  , typeSourceCodeText = "String"
                  , typeIsAppStateType = False
                  }
                , { name = "stateBefore"
                  , typeSourceCodeText = "Backend.State.State"
                  , typeIsAppStateType = True
                  }
                ]
          }
        , { functionName = "Backend.ExposeFunctionsToAdmin.function_report"
          , parameters =
                [ { name = "state"
                  , typeSourceCodeText = "Backend.State.State"
                  , typeIsAppStateType = True
                  }
                ]
          }
        ]
    }
