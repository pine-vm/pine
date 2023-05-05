module UserAccount exposing
    ( CreditBalanceEvent
    , CreditBalanceEventOrigin(..)
    , ReceiveCreditsForOtherReasonStruct
    , addCreditBalanceEvent
    , getUserAccountRemainingCredits
    )


type alias CreditBalanceEvent =
    { posixTime : Int
    , addedCredits : Int
    , origin : CreditBalanceEventOrigin
    , resultCreditBalance : Int
    }


type CreditBalanceEventOrigin
    = PayForItem { itemId : String }
    | RedeemCreditVoucherCode String
    | ReceiveCreditsForOtherReason ReceiveCreditsForOtherReasonStruct


type alias ReceiveCreditsForOtherReasonStruct =
    { reasonText : String
    }


getUserAccountRemainingCredits : { a | creditBalanceEvents : List CreditBalanceEvent } -> Int
getUserAccountRemainingCredits =
    .creditBalanceEvents >> List.head >> Maybe.map .resultCreditBalance >> Maybe.withDefault 0


addCreditBalanceEvent :
    { posixTime : Int, addedCredits : Int, origin : CreditBalanceEventOrigin }
    -> { a | creditBalanceEvents : List CreditBalanceEvent }
    -> { a | creditBalanceEvents : List CreditBalanceEvent }
addCreditBalanceEvent { posixTime, addedCredits, origin } userAccountBefore =
    let
        creditBalanceBefore =
            userAccountBefore |> getUserAccountRemainingCredits

        creditBalanceEvents =
            { posixTime = posixTime
            , addedCredits = addedCredits
            , origin = origin
            , resultCreditBalance = creditBalanceBefore + addedCredits
            }
                :: userAccountBefore.creditBalanceEvents
    in
    { userAccountBefore | creditBalanceEvents = creditBalanceEvents }
