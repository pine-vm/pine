module Simplify exposing (State, simplify)

{-| In this module we're interpreting `SimplifyCmd`s.

We generate the possible Cmds via `Simplify.Cmd.cmdsForRun` and then run them
one by one, keeping the currently best RandomRun + the value it generates.

A small pattern has arisen when trying to conditionally run further
simplifications: the function `keepIfBetter` returns information about whether
the new RandomRun candidate was better than the previous one or not, inside
the `wasImprovement` field, alongside the `newState`, which is either the old
state or a new one based on the candidate RandomRun. The `wasImprovement` bool
lets you branch out and do some simplifications only if others have succeeded or
failed.

Sometimes you want to do multiple simplification attempts unconditionally. For
those cases you can do `andThen`:

    state
        |> simplifyFoo
        |> andThen simplifyBar
        |> andThen simplifyBaz

-}

import Fuzz.Float
import Fuzz.Internal exposing (Fuzzer)
import GenResult exposing (GenResult(..))
import PRNG
import RandomRun exposing (Chunk, RandomRun)
import Simplify.Cmd exposing (SimplifyCmd, SimplifyCmdType(..))
import Test.Expectation exposing (Expectation(..))


type alias State a =
    { getExpectation : a -> Expectation
    , fuzzer : Fuzzer a
    , value : a
    , randomRun : RandomRun
    , expectation : Expectation
    }


type alias SimplifyResult a =
    { wasImprovement : Bool
    , newState : State a
    }


noImprovement : State a -> SimplifyResult a
noImprovement state =
    { wasImprovement = False
    , newState = state
    }


andThen : (State a -> SimplifyResult a) -> SimplifyResult a -> SimplifyResult a
andThen fn { newState } =
    fn newState


simplify : State a -> ( a, RandomRun, Expectation )
simplify state =
    {-
       let
           _ =
               Debug.log "--------------" ()
       in
    -}
    if RandomRun.isEmpty state.randomRun then
        -- We can't do any better
        ( state.value, state.randomRun, state.expectation )

    else
        simplifyWhileProgress state


simplifyWhileProgress : State a -> ( a, RandomRun, Expectation )
simplifyWhileProgress state =
    {-
       let
           _ =
               logState "\ncurrent best" state
       in
    -}
    let
        nextState =
            simplifyOnce state
    in
    if RandomRun.equal nextState.randomRun state.randomRun then
        ( nextState.value, nextState.randomRun, nextState.expectation )

    else
        simplifyWhileProgress nextState


simplifyOnce : State a -> State a
simplifyOnce state =
    runCmds
        (Simplify.Cmd.cmdsForRun state.randomRun)
        state


runCmds : List SimplifyCmd -> State a -> State a
runCmds cmds state =
    case cmds of
        [] ->
            state

        cmd :: rest ->
            let
                { wasImprovement, newState } =
                    runCmd cmd state

                newLength =
                    RandomRun.length newState.randomRun

                newRest =
                    if wasImprovement && newLength < RandomRun.length state.randomRun then
                        rest
                            |> List.filter (\{ minLength } -> newLength >= minLength)

                    else
                        rest
            in
            runCmds newRest newState


logRun : String -> RandomRun -> RandomRun
logRun label run =
    let
        _ =
            Debug.log label (RandomRun.toList run)
    in
    run


logState : String -> State a -> State a
logState label state =
    let
        runString =
            Debug.toString (RandomRun.toList state.randomRun)
    in
    let
        _ =
            case Fuzz.Internal.generate (PRNG.hardcoded state.randomRun) state.fuzzer of
                Generated { value } ->
                    let
                        _ =
                            Debug.log (label ++ " - " ++ runString ++ " --->") value
                    in
                    ()

                _ ->
                    ()
    in
    state


runCmd : SimplifyCmd -> State a -> SimplifyResult a
runCmd cmd state =
    let
        {-
           _ =
               logRun ("trying " ++ Debug.toString cmd.type_ ++ " on") state.randomRun
        -}
        result =
            case cmd.type_ of
                DeleteChunkAndMaybeDecrementPrevious chunk ->
                    deleteChunkAndMaybeDecrementPrevious chunk state

                ReplaceChunkWithZero chunk ->
                    replaceChunkWithZero chunk state

                SortChunk chunk ->
                    sortChunk chunk state

                MinimizeFloat options ->
                    minimizeFloat options state

                MinimizeChoice options ->
                    minimizeChoice options state

                RedistributeChoicesAndMaybeIncrement options ->
                    redistributeChoicesAndMaybeIncrement options state

                DecrementTogether options ->
                    decrementTogether options state

                SwapChunkWithNeighbour chunk ->
                    swapChunkWithNeighbour chunk state
    in
    {-
       let
           _ =
               if result.wasImprovement then
                   logState "Success" result.newState

               else
                   result.newState
       in
    -}
    result


{-| Tries the new RandomRun with the given fuzzer and test fn, and if the run
generates a value which fails the test, save it as the currently best
counterexample.
-}
keepIfBetter : RandomRun -> State a -> SimplifyResult a
keepIfBetter newRandomRun state =
    if RandomRun.equal state.randomRun newRandomRun then
        noImprovement state

    else
        case Fuzz.Internal.generate (PRNG.hardcoded newRandomRun) state.fuzzer of
            Generated { value } ->
                case state.getExpectation value of
                    Pass _ ->
                        noImprovement state

                    Fail fail ->
                        if RandomRun.compare state.randomRun newRandomRun == GT then
                            { wasImprovement = True
                            , newState =
                                { state
                                    | value = value
                                    , randomRun = newRandomRun
                                    , expectation = Fail fail
                                }
                            }

                        else
                            noImprovement state

            Rejected _ ->
                noImprovement state



-- SIMPLIFY CMD IMPLEMENTATIONS


deleteChunkAndMaybeDecrementPrevious : Chunk -> State a -> SimplifyResult a
deleteChunkAndMaybeDecrementPrevious chunk state =
    let
        runWithDelete : RandomRun
        runWithDelete =
            state.randomRun
                |> RandomRun.deleteChunk chunk

        runWithDeleteAndDecrement : RandomRun
        runWithDeleteAndDecrement =
            runWithDelete
                |> RandomRun.update (chunk.startIndex - 1) (\x -> x - 1)

        afterDeleteAndDecrement =
            keepIfBetter runWithDeleteAndDecrement state
    in
    if afterDeleteAndDecrement.wasImprovement then
        afterDeleteAndDecrement

    else
        keepIfBetter runWithDelete state


replaceChunkWithZero : Chunk -> State a -> SimplifyResult a
replaceChunkWithZero chunk state =
    let
        simplifiedRun : RandomRun
        simplifiedRun =
            RandomRun.replaceChunkWithZero chunk state.randomRun
    in
    keepIfBetter simplifiedRun state


sortChunk : Chunk -> State a -> SimplifyResult a
sortChunk chunk state =
    let
        simplifiedRun : RandomRun
        simplifiedRun =
            RandomRun.sortChunk chunk state.randomRun
    in
    keepIfBetter simplifiedRun state


swapChunkWithNeighbour : Chunk -> State a -> SimplifyResult a
swapChunkWithNeighbour chunk state =
    let
        otherChunk : Chunk
        otherChunk =
            { size = chunk.size
            , startIndex = chunk.startIndex + chunk.size
            }
    in
    state.randomRun
        |> RandomRun.swapChunks
            { leftChunk = chunk
            , rightChunk = otherChunk
            }
        |> Maybe.map (\simplifiedRun -> keepIfBetter simplifiedRun state)
        |> Maybe.withDefault (noImprovement state)


minimizeFloat : { leftIndex : Int } -> State a -> SimplifyResult a
minimizeFloat { leftIndex } state =
    case RandomRun.get leftIndex state.randomRun of
        Nothing ->
            noImprovement state

        Just hi_ ->
            if Fuzz.Float.isFractional hi_ then
                let
                    minimizeExponentPart : State a -> SimplifyResult a
                    minimizeExponentPart state_ =
                        case
                            Maybe.map2 Tuple.pair
                                (RandomRun.get leftIndex state_.randomRun)
                                (RandomRun.get (leftIndex + 1) state_.randomRun)
                        of
                            Nothing ->
                                noImprovement state_

                            Just ( hi, lo ) ->
                                let
                                    exponent : Int
                                    exponent =
                                        Fuzz.Float.getExponent ( hi, lo )
                                in
                                binarySearchShrink
                                    { low = 0
                                    , high = exponent
                                    , state = state_
                                    , updateRun =
                                        \newExponent accRun ->
                                            let
                                                ( newHi, newLo ) =
                                                    Fuzz.Float.setExponent newExponent ( hi, lo )
                                            in
                                            accRun
                                                |> RandomRun.set leftIndex newHi
                                                |> RandomRun.set (leftIndex + 1) newLo
                                    }

                    minimizeMantissaPart : State a -> SimplifyResult a
                    minimizeMantissaPart state_ =
                        case
                            Maybe.map2 Tuple.pair
                                (RandomRun.get leftIndex state_.randomRun)
                                (RandomRun.get (leftIndex + 1) state_.randomRun)
                        of
                            Nothing ->
                                noImprovement state_

                            Just ( hi, lo ) ->
                                let
                                    mantissa : Int
                                    mantissa =
                                        Fuzz.Float.getMantissa ( hi, lo )
                                in
                                binarySearchShrink
                                    { low = 0
                                    , high = mantissa
                                    , state = state_
                                    , updateRun =
                                        \newMantissa accRun ->
                                            let
                                                ( newHi, newLo ) =
                                                    Fuzz.Float.setMantissa newMantissa ( hi, lo )
                                            in
                                            accRun
                                                |> RandomRun.set leftIndex newHi
                                                |> RandomRun.set (leftIndex + 1) newLo
                                    }
                in
                state
                    |> minimizeExponentPart
                    |> andThen minimizeMantissaPart

            else
                noImprovement state


minimizeChoice : { index : Int } -> State a -> SimplifyResult a
minimizeChoice { index } state =
    case RandomRun.get index state.randomRun of
        Nothing ->
            noImprovement state

        Just value ->
            if value == 0 then
                noImprovement state

            else
                binarySearchShrink
                    { low = 0
                    , high = value
                    , state = state
                    , updateRun =
                        \value_ accRun ->
                            RandomRun.set index value_ accRun
                    }


decrementTogether : { leftIndex : Int, rightIndex : Int, by : Int } -> State a -> SimplifyResult a
decrementTogether { leftIndex, rightIndex, by } state =
    let
        simplifiedRun : RandomRun
        simplifiedRun =
            state.randomRun
                |> RandomRun.update leftIndex (\n -> n - by)
                |> RandomRun.update rightIndex (\n -> n - by)
    in
    keepIfBetter simplifiedRun state


redistributeChoicesAndMaybeIncrement : { leftIndex : Int, rightIndex : Int } -> State a -> SimplifyResult a
redistributeChoicesAndMaybeIncrement options state =
    {- First we try swapping them if left > right.

       Then we try to (binary-search) minimize the left while keeping the
       sum constant (so what we subtract from left we add to right).
    -}
    case RandomRun.swapIfOutOfOrder options state.randomRun of
        Nothing ->
            {- This only happens if we can't find the values (indexes are out of
               order). Both "did swap" and "didn't swap" fall into the Just case.
            -}
            noImprovement state

        Just { newRun, newLeftValue, newRightValue } ->
            let
                ({ newState } as afterSwap) =
                    keepIfBetter newRun state

                go : RandomRun -> SimplifyResult a
                go initialRun =
                    binarySearchShrink
                        { low = 0
                        , high = newLeftValue
                        , state = { newState | randomRun = initialRun }
                        , updateRun =
                            \value accRun ->
                                RandomRun.replace
                                    [ ( options.leftIndex, value )
                                    , ( options.rightIndex, newRightValue + newLeftValue - value )
                                    ]
                                    accRun
                        }

                afterShrinkAlone =
                    keepIfBetter (go newState.randomRun).newState.randomRun newState
            in
            if afterShrinkAlone.wasImprovement then
                afterShrinkAlone

            else
                let
                    {- This is to play nicer with `intFrequency`.

                       Imagine a test that there exists no list with sum > 1000.
                       An obvious counterexample is [1001].

                       With our "flip-a-coin" generated lists (RandomRuns like
                       [1,…,1,…,1,…,1,…,0]) and integers that prefer smaller
                       values (thus using intFrequency to bucket into various
                       sizes (RandomRuns like [bucket,int] -- eg. [0,14] or
                       [1,258]), it's no longer enough to just redistribute
                       values between the "actual integer" parts. The bucket
                       would actively stop you, because [1,258] still results in
                       a value < 255, and thus the test would stop failing.

                       What you need to do is to "promote" the bucket while
                       increasing the "actual integer". Thus, with a starting
                       state:

                       value = [236, 255], random run: [0,236, 0,255]

                       we need to try to increment the third integer in the
                       RandomRun (the bucket of the second value):

                       value = [236, 255], random run: [0,236, 1,255]

                       This will allow us to redistribute while still failing:

                       value = [0, 491], random run: [0,0, 1,491]

                       Which will allow other Simplify Cmds to remove
                       the leading 0 etc...

                       We'll first try without the increment, and if there is no
                       improvement, we try with the increment.
                    -}
                    runWithIncrementedRightBucket : RandomRun
                    runWithIncrementedRightBucket =
                        newState.randomRun
                            |> RandomRun.update (options.rightIndex - 1) (\x -> x + 1)

                    afterIncrementAndShrink =
                        keepIfBetter (go runWithIncrementedRightBucket).newState.randomRun newState
                in
                if afterIncrementAndShrink.wasImprovement then
                    afterIncrementAndShrink

                else
                    afterSwap



-- BINARY SEARCH SHRINKING


type alias BinarySearchOptions a =
    { low : Int
    , high : Int
    , state : State a
    , updateRun : Int -> RandomRun -> RandomRun
    }


binarySearchShrink : BinarySearchOptions a -> SimplifyResult a
binarySearchShrink ({ updateRun, low, state } as options) =
    let
        -- Let's try the best case first
        runWithLow =
            updateRun low options.state.randomRun

        afterLow =
            keepIfBetter runWithLow state
    in
    if afterLow.wasImprovement then
        -- We can't do any better
        afterLow

    else
        binarySearchLoop { wasImprovement = False } options


binarySearchLoop : { wasImprovement : Bool } -> BinarySearchOptions a -> SimplifyResult a
binarySearchLoop old ({ low, high, state, updateRun } as options) =
    if low + 1 < high then
        let
            mid =
                {- `(low + high) // 2` would cause integer overflow

                   `(low + (high - low) // 2)` would use `truncate` which
                   converts to 32bit and has caused this binaryShrinkLoop inside
                   MinimizeFloat to loop infinitely in the past.
                -}
                low + round ((toFloat high - toFloat low) / 2)

            newRun =
                updateRun mid options.state.randomRun

            afterMid =
                keepIfBetter newRun state

            optionsWithNewRange =
                if afterMid.wasImprovement then
                    { options | high = mid }

                else
                    { options | low = mid }

            newOptions =
                { optionsWithNewRange | state = afterMid.newState }
        in
        binarySearchLoop { wasImprovement = afterMid.wasImprovement } newOptions

    else
        { wasImprovement = old.wasImprovement
        , newState = options.state
        }
