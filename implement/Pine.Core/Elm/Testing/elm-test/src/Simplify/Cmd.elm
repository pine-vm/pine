module Simplify.Cmd exposing
    ( SimplifyCmd
    , SimplifyCmdType(..)
    , cmdsForRun
    )

import MicroListExtra as List
import RandomRun exposing (Chunk, RandomRun)
import Set exposing (Set)


type alias SimplifyCmd =
    { type_ : SimplifyCmdType
    , minLength : Int
    }


type SimplifyCmdType
    = DeleteChunkAndMaybeDecrementPrevious Chunk
    | ReplaceChunkWithZero Chunk
    | SortChunk Chunk
    | {- 64bit floats are represented by pairs of two 32bit integers (plus one
         0/1 bool-int to the right of them to handle the sign).

         That makes them harder to shrink: the MinimizeChoice shrinker just cares
         about one number and the RedistributeChoices shrinker has the (unhelpful
         for floats) property that the sum of the integers must be preserved.

         What we need is to minimize parts of the integers in separation:

         high 32 bits integer                | low 32 bits integer
         ------------------------------------|------------------------------------
         00000000 00000000 00000000 00000000 | 00000000 00000000 00000000 00000000
         ^            ^^^^ ^^^^^^^^ ^^^^^^^^   ^^^^^^^^ ^^^^^^^^ ^^^^^^^^ ^^^^^^^^
         |^^^^^^^ ^^^^ |
         |  |         mantissa bits (52 in total)
         | exponent bits            (11 in total)
         "is fractional?" bit        (1 in total)

         We can use our trusty binary search algorithm for each of these, but
         it's important to make sure we can transfer mantissa bits between the
         two underlying integers. Thus, we should extract the mantissa from the
         number (JS can handle 52bit integers) and be able to overwrite it:

         Fuzz.Float.getMantissa : (Int, Int) -> Int
         Fuzz.Float.setMantissa : Int -> (Int, Int) -> (Int, Int)

         ----

         The leftIndex in this Cmd relates to the `hi` int. Remember the float
         representation inside RandomRun:

         [ high int (32bit), low int (32bit), sign int (1bit) ]
            ^
            |
           leftIndex

      -}
      MinimizeFloat { leftIndex : Int }
    | MinimizeChoice { index : Int }
    | {- For notes about why the increment, see a comment in
         Simplify.redistributeChoicesAndMaybeIncrement.

         TL;DR: we're trying to increase the success rate of Fuzz.intFrequency
         by incrementing the "bucket index" of the right number.
      -}
      RedistributeChoicesAndMaybeIncrement { leftIndex : Int, rightIndex : Int }
    | {- DecrementTogether is tailored to how we generate ints:
         each Fuzz.int contributes two numbers into the RandomRun:

         [ smallIntFrequencies | uniformInt ]

         If we have two integers next to each other, we can try to decrement them
         both at the same time.

         Also, we're removing one bit from the `uniformInt` value for the
         sign bit, effectively halving the numbers in the process. So it's useful
         to be able to decrement by 2 instead of just by 1.
      -}
      DecrementTogether { leftIndex : Int, rightIndex : Int, by : Int }
    | SwapChunkWithNeighbour Chunk


cmdsForRun : RandomRun -> List SimplifyCmd
cmdsForRun run =
    let
        length =
            RandomRun.length run
    in
    List.fastConcat
        [ deletionCmds length
        , zeroCmds length
        , minimizeChoiceCmds run length
        , minimizeFloatCmds run length
        , sortCmds length
        , redistributeCmds length
        , decrementTogetherCmds length
        , swapCmds length
        ]


deletionCmds : Int -> List SimplifyCmd
deletionCmds length =
    chunkCmds
        DeleteChunkAndMaybeDecrementPrevious
        { length = length
        , allowChunksOfSize1 = True
        }


zeroCmds : Int -> List SimplifyCmd
zeroCmds length =
    chunkCmds
        ReplaceChunkWithZero
        { length = length
        , allowChunksOfSize1 = False -- already happens in binary search
        }


sortCmds : Int -> List SimplifyCmd
sortCmds length =
    chunkCmds
        SortChunk
        { length = length
        , allowChunksOfSize1 = False -- doesn't make sense for sorting
        }


minimizeChoiceCmds : RandomRun -> Int -> List SimplifyCmd
minimizeChoiceCmds run length =
    run
        |> RandomRun.toList
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( index, value ) ->
                if value > 0 then
                    Just
                        { type_ = MinimizeChoice { index = index }
                        , minLength = index + 1
                        }

                else
                    -- no point in trying to minimize a 0
                    Nothing
            )


minimizeFloatCmds : RandomRun -> Int -> List SimplifyCmd
minimizeFloatCmds run length =
    let
        possibleBoolIndexes : Set Int
        possibleBoolIndexes =
            run
                |> RandomRun.toList
                |> List.indexedMap Tuple.pair
                |> List.filterMap
                    (\( index, value ) ->
                        if value > 1 then
                            Nothing

                        else
                            Just index
                    )
                |> Set.fromList
    in
    List.range 0 (length - 3)
        |> List.filterMap
            (\index ->
                if Set.member (index + 2) possibleBoolIndexes then
                    Just
                        { type_ = MinimizeFloat { leftIndex = index }
                        , minLength = index + 3
                        }

                else
                    Nothing
            )


decrementTogetherCmds : Int -> List SimplifyCmd
decrementTogetherCmds length =
    let
        maxOffsetLimit =
            if length < 512 then
                4

            else
                2
    in
    List.range 0 (length - 2)
        |> List.fastConcatMap
            (\index ->
                let
                    maxOffset =
                        min
                            maxOffsetLimit
                            (length - index - 1)
                in
                List.range 1 maxOffset
                    |> List.fastConcatMap
                        (\offset ->
                            [ 4, 2, 1 ]
                                |> List.map
                                    (\by ->
                                        let
                                            rightIndex =
                                                index + offset
                                        in
                                        { type_ =
                                            DecrementTogether
                                                { leftIndex = index
                                                , rightIndex = rightIndex
                                                , by = by
                                                }
                                        , minLength = rightIndex + 1
                                        }
                                    )
                        )
            )


redistributeCmds : Int -> List SimplifyCmd
redistributeCmds length =
    let
        forOffset : Int -> List SimplifyCmd
        forOffset offset =
            if offset >= length then
                []

            else
                List.range 0 (length - 1 - offset)
                    |> List.reverse
                    |> List.map
                        (\leftIndex ->
                            { type_ =
                                RedistributeChoicesAndMaybeIncrement
                                    { leftIndex = leftIndex
                                    , rightIndex = leftIndex + offset
                                    }
                            , minLength = leftIndex + offset + 1
                            }
                        )
    in
    forOffset 3 ++ forOffset 2 ++ forOffset 1


swapCmds : Int -> List SimplifyCmd
swapCmds length =
    chunkCmds
        SwapChunkWithNeighbour
        { length = length
        , allowChunksOfSize1 = False -- other Cmds are already doing the case with size=1
        }
        |> List.map
            (\cmd ->
                case cmd.type_ of
                    SwapChunkWithNeighbour chunk ->
                        { cmd | minLength = cmd.minLength + chunk.size }

                    _ ->
                        cmd
            )


{-| Will generate SimplifyCmds for all chunks of sizes 1,2,4,8 in bounds of the
given RandomRun length.

chunkCmds
SortChunk
{ itemsCount = 10, allowChunksOfSize1 = False }
-->
[ -- Chunks of size 8
SortChunk { chunkSize = 8, startIndex = 2 } -- [..XXXXXXXX]
, SortChunk { chunkSize = 8, startIndex = 1 } -- [.XXXXXXXX.]
, SortChunk { chunkSize = 8, startIndex = 0 } -- [XXXXXXXX..]

    -- Chunks of size 4
    , SortChunk { chunkSize = 4, startIndex = 6 } -- [......XXXX]
    , SortChunk { chunkSize = 4, startIndex = 5 } -- [.....XXXX.]
    , -- ...
      SortChunk { chunkSize = 4, startIndex = 1 } -- [.XXXX.....]
    , SortChunk { chunkSize = 4, startIndex = 0 } -- [XXXX......]

    -- Chunks of size 3
    , SortChunk { chunkSize = 3, startIndex = 7 } -- [.......XXX]
    , SortChunk { chunkSize = 3, startIndex = 6 } -- [......XXX.]
    , -- ...
      SortChunk { chunkSize = 3, startIndex = 1 } -- [.XXX......]
    , SortChunk { chunkSize = 3, startIndex = 0 } -- [XXX.......]

    -- Chunks of size 2
    , SortChunk { chunkSize = 2, startIndex = 8 } -- [........XX]
    , SortChunk { chunkSize = 2, startIndex = 7 } -- [.......XX.]
    , -- ...
      SortChunk { chunkSize = 2, startIndex = 1 } -- [.XX.......]
    , SortChunk { chunkSize = 2, startIndex = 0 } -- [XX........]
    ]

-}
chunkCmds :
    ({ size : Int, startIndex : Int } -> SimplifyCmdType)
    -> { length : Int, allowChunksOfSize1 : Bool }
    -> List SimplifyCmd
chunkCmds toType { length, allowChunksOfSize1 } =
    let
        initChunkSize : Int
        initChunkSize =
            if allowChunksOfSize1 then
                1

            else
                2

        go : Int -> Int -> List SimplifyCmd -> List SimplifyCmd
        go chunkSize startIndex acc =
            if startIndex > length - chunkSize then
                if chunkSize == 8 then
                    acc

                else if chunkSize == 2 || chunkSize == 3 then
                    {- Chunks of 3 are common:
                       [ list: 1 | int bucket: 0 | int value: 0 ]
                    -}
                    go (chunkSize + 1) 0 acc

                else
                    go (chunkSize * 2) 0 acc

            else
                let
                    newCmd =
                        { type_ =
                            toType
                                { size = chunkSize
                                , startIndex = startIndex
                                }
                        , minLength = startIndex + chunkSize
                        }
                in
                go chunkSize (startIndex + 1) (newCmd :: acc)
    in
    go initChunkSize 0 []
