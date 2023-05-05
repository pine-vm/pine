module Backend.ExposeFunctionsToAdmin exposing (..)

import Backend.State
import Dict
import UserAccount


listNewUserAccounts :
    { accountMaximumAgeInDays : Int }
    -> Backend.State.State
    -> List { accountId : String, accountAgeInDays : Int }
listNewUserAccounts { accountMaximumAgeInDays } state =
    state.usersAccounts
        |> Dict.toList
        |> List.map
            (\( userAccountId, userAccount ) ->
                let
                    accountAgeInDays =
                        (state.posixTimeMilli // 1000 - userAccount.creationPosixTime) // 60 // 60 // 24
                in
                { accountId = userAccountId
                , accountAgeInDays = accountAgeInDays
                }
            )
        |> List.filter (\{ accountAgeInDays } -> accountAgeInDays <= accountMaximumAgeInDays)
        |> List.sortBy .accountAgeInDays


deleteUserAccountById : String -> Backend.State.State -> Backend.State.State
deleteUserAccountById userAccountId stateBefore =
    { stateBefore
        | usersAccounts = stateBefore.usersAccounts |> Dict.remove userAccountId
    }


userAccountAddCreditBalanceEvent :
    { userAccountId : String, addedCredits : Int, reasonText : String }
    -> Backend.State.State
    -> Backend.State.State
userAccountAddCreditBalanceEvent { userAccountId, addedCredits, reasonText } stateBefore =
    case stateBefore.usersAccounts |> Dict.get userAccountId of
        Nothing ->
            stateBefore

        Just userAccountBefore ->
            let
                origin =
                    UserAccount.ReceiveCreditsForOtherReason { reasonText = reasonText }

                userAccount =
                    userAccountBefore
                        |> UserAccount.addCreditBalanceEvent
                            { posixTime = stateBefore.posixTimeMilli // 1000
                            , addedCredits = addedCredits
                            , origin = origin
                            }
            in
            { stateBefore
                | usersAccounts = stateBefore.usersAccounts |> Dict.insert userAccountId userAccount
            }
