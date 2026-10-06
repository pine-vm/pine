module RandomRun exposing
    ( Chunk
    , RandomRun
    , append
    , compare
    , deleteChunk
    , empty
    , equal
    , get
    , isEmpty
    , length
    , nextChoice
    , replace
    , replaceChunkWithZero
    , set
    , sortChunk
    , swapChunks
    , swapIfOutOfOrder
    , toList
    , update
    )

import MicroListExtra as List
import Queue exposing (Queue)


type alias RandomRun =
    { data : Queue Int

    -- derived precomputed data:
    , length : Int
    }


type alias Chunk =
    { size : Int
    , startIndex : Int
    }


empty : RandomRun
empty =
    { data = Queue.empty
    , length = 0
    }


isEmpty : RandomRun -> Bool
isEmpty run =
    run.length == 0


nextChoice : RandomRun -> Maybe ( Int, RandomRun )
nextChoice run =
    case Queue.dequeue run.data of
        ( Nothing, _ ) ->
            Nothing

        ( Just first, rest ) ->
            Just
                ( first
                , { run
                    | length = run.length - 1
                    , data = rest
                  }
                )


append : Int -> RandomRun -> RandomRun
append n run =
    { run
        | length = run.length + 1
        , data = Queue.enqueue n run.data
    }


isInBounds : Chunk -> RandomRun -> Bool
isInBounds { startIndex, size } run =
    startIndex + size <= run.length


length : RandomRun -> Int
length run =
    run.length


getChunk : Chunk -> RandomRun -> Maybe (List Int)
getChunk chunk run =
    if isInBounds chunk run then
        run.data
            |> Queue.toList
            |> List.drop chunk.startIndex
            |> List.take chunk.size
            |> Just

    else
        Nothing


deleteChunk : Chunk -> RandomRun -> RandomRun
deleteChunk chunk run =
    if isInBounds chunk run then
        let
            list =
                Queue.toList run.data

            result =
                { run
                    | length = run.length - chunk.size
                    , data =
                        (List.take chunk.startIndex list
                            ++ List.drop (chunk.startIndex + chunk.size) list
                        )
                            |> Queue.fromList
                }
        in
        result

    else
        run


replaceChunkWithZero : Chunk -> RandomRun -> RandomRun
replaceChunkWithZero chunk run =
    if isInBounds chunk run then
        -- TODO PERF: maybe `replace [...] run` would be faster?
        let
            list =
                Queue.toList run.data
        in
        { run
            | data =
                List.fastConcat
                    [ List.take chunk.startIndex list
                    , List.repeat chunk.size 0
                    , List.drop (chunk.startIndex + chunk.size) list
                    ]
                    |> Queue.fromList
        }

    else
        run


sortChunk : Chunk -> RandomRun -> RandomRun
sortChunk chunk run =
    case getChunk chunk run of
        Nothing ->
            run

        Just chunkData ->
            let
                sortedIndexed : List ( Int, Int )
                sortedIndexed =
                    chunkData
                        |> List.sort
                        |> List.indexedMap
                            (\i value -> ( chunk.startIndex + i, value ))
            in
            replace sortedIndexed run


replace : List ( Int, Int ) -> RandomRun -> RandomRun
replace values run =
    replaceInList values run.length (Queue.toList run.data)


{-| An optimization to not do Queue.toList redundantly.

Expects `list == Queue.toList run.data`
and `len == Queue.size run.data`

-}
replaceInList : List ( Int, Int ) -> Int -> List Int -> RandomRun
replaceInList values len list =
    { length = len
    , data =
        List.foldl
            (\( index, newValue ) accList ->
                if newValue < 0 then
                    accList

                else
                    List.setAt index newValue len accList
            )
            list
            values
            |> Queue.fromList
    }


swapChunks :
    { leftChunk : Chunk, rightChunk : Chunk }
    -> RandomRun
    -> Maybe RandomRun
swapChunks { leftChunk, rightChunk } run =
    let
        list =
            Queue.toList run.data
    in
    Maybe.map2
        (\lefts rights ->
            replaceInList
                (List.concat
                    [ List.indexedMap (\i n -> ( rightChunk.startIndex + i, n )) lefts
                    , List.indexedMap (\i n -> ( leftChunk.startIndex + i, n )) rights
                    ]
                )
                run.length
                list
        )
        {- TODO PERF: both of these are doing the Queue.toList etc. operations
           while we already have that factored out in the `list` var.
           We could factor that operation out `getChunk`?
        -}
        (getChunk leftChunk run)
        (getChunk rightChunk run)


swapIfOutOfOrder :
    { leftIndex : Int, rightIndex : Int }
    -> RandomRun
    ->
        Maybe
            { newRun : RandomRun
            , newLeftValue : Int
            , newRightValue : Int
            }
swapIfOutOfOrder { leftIndex, rightIndex } run =
    let
        list =
            Queue.toList run.data
    in
    Maybe.map2
        (\left right ->
            if left > right then
                { newRun =
                    replaceInList
                        [ ( leftIndex, right )
                        , ( rightIndex, left )
                        ]
                        run.length
                        list
                , newLeftValue = right
                , newRightValue = left
                }

            else
                { newRun = run
                , newLeftValue = left
                , newRightValue = right
                }
        )
        (List.getAt leftIndex list)
        (List.getAt rightIndex list)


get : Int -> RandomRun -> Maybe Int
get index run =
    run.data
        |> Queue.toList
        |> List.getAt index


set : Int -> Int -> RandomRun -> RandomRun
set index value run =
    if run.length <= index then
        run

    else
        { run
            | data =
                run.data
                    |> Queue.toList
                    |> List.setAt index value run.length
                    |> Queue.fromList
        }


sortKey : RandomRun -> ( Int, List Int )
sortKey run =
    ( run.length
    , toList run
    )


compare : RandomRun -> RandomRun -> Order
compare a b =
    Basics.compare (sortKey a) (sortKey b)


toList : RandomRun -> List Int
toList run =
    Queue.toList run.data


update : Int -> (Int -> Int) -> RandomRun -> RandomRun
update index fn run =
    case get index run of
        Nothing ->
            run

        Just value ->
            replace [ ( index, fn value ) ] run


equal : RandomRun -> RandomRun -> Bool
equal run1 run2 =
    toList run1 == toList run2
