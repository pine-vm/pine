module Backend.State exposing (..)

import Dict
import UserAccount


type alias State =
    { posixTimeMilli : Int
    , usersAccounts : Dict.Dict String UserAccountState
    }


type alias UserAccountState =
    { creationPosixTime : Int
    , creditBalanceEvents : List UserAccount.CreditBalanceEvent
    }
