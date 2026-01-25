module Rope exposing (Rope, RopeFilled(..), empty, filledPrependTo, one, prependTo, prependToFilled, toList)

{-| inspired by [miniBill/elm-rope](https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/)
-}


type alias Rope a =
    Maybe (RopeFilled a)


type RopeFilled a
    = Leaf a ()
    | Branch2 (RopeFilled a) (RopeFilled a)


empty : Rope a
empty =
    Nothing


one : a -> RopeFilled a
one onlyElement =
    Leaf onlyElement ()


filledPrependTo : Rope a -> RopeFilled a -> Rope a
filledPrependTo right leftLikelyFilled =
    case right of
        Nothing ->
            Just leftLikelyFilled

        Just rightLikelyFilled ->
            Just (Branch2 leftLikelyFilled rightLikelyFilled)


prependToFilled : RopeFilled a -> Rope a -> Rope a
prependToFilled rightLikelyFilled left =
    case left of
        Nothing ->
            Just rightLikelyFilled

        Just leftLikelyFilled ->
            Just (Branch2 leftLikelyFilled rightLikelyFilled)


prependTo : Rope a -> Rope a -> Rope a
prependTo right left =
    case left of
        Nothing ->
            right

        Just leftLikelyFilled ->
            case right of
                Nothing ->
                    left

                Just rightLikelyFilled ->
                    Just (Branch2 leftLikelyFilled rightLikelyFilled)


toList : Rope a -> List a
toList rope =
    case rope of
        Nothing ->
            []

        Just ropeLikelyFilled ->
            ropeLikelyFilledToListInto [] ropeLikelyFilled


ropeLikelyFilledToListInto : List a -> RopeFilled a -> List a
ropeLikelyFilledToListInto initialAcc ropeLikelyFilled =
    case ropeLikelyFilled of
        Leaf onlyElement () ->
            onlyElement :: initialAcc

        Branch2 left right ->
            ropeLikelyFilledToListInto
                (ropeLikelyFilledToListInto
                    initialAcc
                    right
                )
                left
