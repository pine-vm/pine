module Backend.Main exposing (webServiceMain)

import Backend.State
import Dict
import Platform.WebService


type alias State =
    Backend.State.State


type Event
    = HttpRequestEvent Platform.WebService.HttpRequestEventStruct


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( initState, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions state =
    { httpRequest = HttpRequestEvent >> processEvent
    , posixTimeIsPast = Nothing
    }


processEvent : Event -> State -> ( State, Platform.WebService.Commands State )
processEvent _ stateBefore =
    ( stateBefore, [] )


initState : State
initState =
    { posixTimeMilli = 1000 * 60 * 60 * 24 * 2
    , usersAccounts =
        [ ( "first-user"
          , { creationPosixTime = 60 * 60 * 4
            , creditBalanceEvents = []
            }
          )
        , ( "second-user"
          , { creationPosixTime = 60 * 60 * 33
            , creditBalanceEvents = []
            }
          )
        ]
            |> Dict.fromList
    }
