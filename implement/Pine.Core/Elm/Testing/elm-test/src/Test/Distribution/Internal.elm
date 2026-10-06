module Test.Distribution.Internal exposing
    ( Distribution(..)
    , ExpectedDistribution(..)
    , expectedDistributionToString
    , formatPct
    , getDistributionLabels
    , getExpectedDistributions
    , insufficientlyCovered
    , sufficientlyCovered
    )


type Distribution a
    = NoDistributionNeeded
    | ReportDistribution (List ( String, a -> Bool ))
    | ExpectDistribution (List ( ExpectedDistribution, String, a -> Bool ))


type ExpectedDistribution
    = Zero
    | MoreThanZero
    | AtLeast Float


expectedDistributionToString : ExpectedDistribution -> String
expectedDistributionToString expectedDistribution =
    case expectedDistribution of
        Zero ->
            "0%"

        MoreThanZero ->
            "> 0%"

        AtLeast pct ->
            ">= " ++ formatPct pct


getDistributionLabels : Distribution a -> Maybe (List ( String, a -> Bool ))
getDistributionLabels distribution =
    case distribution of
        NoDistributionNeeded ->
            Nothing

        ReportDistribution list ->
            Just list

        ExpectDistribution list ->
            Just (List.map (\( _, l, p ) -> ( l, p )) list)


getExpectedDistributions : Distribution a -> Maybe (List ( String, ExpectedDistribution ))
getExpectedDistributions distribution =
    case distribution of
        NoDistributionNeeded ->
            Nothing

        ReportDistribution _ ->
            Nothing

        ExpectDistribution list ->
            Just (List.map (\( e, l, _ ) -> ( l, e )) list)


formatPct : Float -> String
formatPct n =
    let
        intPart : Int
        intPart =
            floor n

        thousandths : Int
        thousandths =
            round (n * 1000 - toFloat (intPart * 1000))
    in
    String.fromInt intPart
        ++ "."
        ++ String.padLeft 3 '0' (String.fromInt thousandths)
        ++ "%"


{-| Distribution checks will be allowed to give a false positive once in `certainty`
runs (that is, with probability `1/certainty`).

The current number was taken from Haskell QuickCheck. Their documentation says
(quote):

> If you are using 'checkDistribution' as part of a test suite, you should
> be careful not to set @certainty@ too low. If you want, say, a 1% chance
> of a false positive during a project's lifetime, then @certainty@ should
> be set to at least @100 \* m \* n@, where @m@ is the number of uses of
> 'cover' in the test suite, and @n@ is the number of times you expect the
> test suite to be run during the project's lifetime. The default value
> is chosen to be big enough for most projects.

In the future we might want to make it configurable, as it is a knob to make
tests finish faster at the expense of larger probability of false positives.

-}
certainty : Int
certainty =
    10 ^ 9


falsePositiveProb : Float
falsePositiveProb =
    1 / toFloat certainty


{-| We will not reject distribution levels that are only slightly below the required
levels. For distributions `AtLeast percentage`, we will accept
`tolerance * percentage`.

In the future we might want to make it configurable, as it is a knob to make
tests finish faster at the expense of larger probability of false positives.

-}
tolerance : Float
tolerance =
    0.9


{-| Accept the distribution if, with some `certainty`, the actual probability is
at least `tolerance` times the required one.

The percentage Float given is in the 0..1 range, not in the 0..100% range.

-}
sufficientlyCovered : Int -> Int -> Float -> Bool
sufficientlyCovered total seen percentage =
    wilsonLow (toFloat seen) (toFloat total) falsePositiveProb
        >= (tolerance * percentage)


{-| The percentage Float given is in the 0..1 range, not in the 0..100% range.
-}
insufficientlyCovered : Int -> Int -> Float -> Bool
insufficientlyCovered total seen percentage =
    wilsonHigh (toFloat seen) (toFloat total) falsePositiveProb
        < percentage


wilsonLow : Float -> Float -> Float -> Float
wilsonLow seen total prob =
    wilson seen total (invnormcdf (prob / 2))


wilsonHigh : Float -> Float -> Float -> Float
wilsonHigh seen total prob =
    wilson seen total (invnormcdf (1 - prob / 2))


{-| <https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval>
-}
wilson : Float -> Float -> Float -> Float
wilson k n z =
    let
        p =
            k / n

        zz =
            z * z
    in
    (p
        + (zz / (2 * n))
        + (z * sqrt ((p * (1 - p) / n) + (zz / (4 * n * n))))
    )
        / (1 + zz / n)


{-| <https://web.archive.org/web/20151110174102/http://home.online.no/~pjacklam/notes/invnorm/>

This approximation is enough for `certainty` <= 10^9.

-}
invnormcdf : Float -> Float
invnormcdf p =
    if p < 0 then
        0 / 0

    else if p > 1 then
        0 / 0

    else if p == 0 then
        -1 / 0

    else if p == 1 then
        1 / 0

    else if p < pLow then
        let
            q =
                sqrt (-2 * logBase e p)
        in
        (((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6)
            / ((((d1 * q + d2) * q + d3) * q + d4) * q + 1)

    else if p <= pHigh then
        let
            q =
                p - 0.5

            r =
                q * q
        in
        ((((((a1 * r + a2) * r + a3) * r + a4) * r + a5) * r + a6) * q)
            / (((((b1 * r + b2) * r + b3) * r + b4) * r + b5) * r + 1)

    else
        let
            q =
                sqrt (-2 * logBase e (1 - p))
        in
        -(((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6)
            / ((((d1 * q + d2) * q + d3) * q + d4) * q + 1)



-- CONSTANTS FOR INVNORMCDF


a1 : Float
a1 =
    -3.969683028665376e1


a2 : Float
a2 =
    2.209460984245205e2


a3 : Float
a3 =
    -2.759285104469687e2


a4 : Float
a4 =
    1.38357751867269e2


a5 : Float
a5 =
    -3.066479806614716e1


a6 : Float
a6 =
    2.506628277459239e0


b1 : Float
b1 =
    -5.447609879822406e1


b2 : Float
b2 =
    1.615858368580409e2


b3 : Float
b3 =
    -1.556989798598866e2


b4 : Float
b4 =
    6.680131188771972e1


b5 : Float
b5 =
    -1.328068155288572e1


c1 : Float
c1 =
    -7.784894002430293e-3


c2 : Float
c2 =
    -3.223964580411365e-1


c3 : Float
c3 =
    -2.400758277161838e0


c4 : Float
c4 =
    -2.549732539343734e0


c5 : Float
c5 =
    4.374664141464968e0


c6 : Float
c6 =
    2.938163982698783e0


d1 : Float
d1 =
    7.784695709041462e-3


d2 : Float
d2 =
    3.224671290700398e-1


d3 : Float
d3 =
    2.445134137142996e0


d4 : Float
d4 =
    3.754408661907416e0


pLow : Float
pLow =
    0.02425


pHigh : Float
pHigh =
    1 - pLow
