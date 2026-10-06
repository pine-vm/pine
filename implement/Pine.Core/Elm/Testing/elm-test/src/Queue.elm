module Queue exposing
    ( Queue, empty, singleton
    , isEmpty, size, enqueue, dequeue, front
    , fromList, toList
    , map, filter, updateFront
    )

{-| NOTE: Vendored from turboMaCk/queue 1.1.0

---

Queue is simple FIFO (first in, first out) datastructure.


# Type

@docs Queue, empty, singleton


# Query

@docs isEmpty, size, enqueue, dequeue, front


# Lists

@docs fromList, toList


# Transformations

@docs map, filter, updateFront

-}

-- Types


type alias Rear a =
    List a


type alias Front a =
    List a


{-| Equality checks (`==`) on `Queue` are unreliable due to dynamic distribution of elements.

If you need equality checks use [`toList`](#toList).

    Queue.toList firstQueue == Queue.toList secondQueue

-}
type Queue a
    = Queue (Front a) (Rear a)


{-| private pseudo-constructor
-}
queue : Front a -> Rear a -> Queue a
queue fl rl =
    case fl of
        [] ->
            Queue (List.reverse rl) []

        _ ->
            Queue fl rl


{-| Construct empty `Queue`
-}
empty : Queue a
empty =
    Queue [] []


{-| Construct Queue containing single value

    Queue.toList (Queue.singleton 1) == [ 1 ]

-}
singleton : a -> Queue a
singleton a =
    Queue [ a ] []



-- Query


{-| Determine if `Queue` is empty

    Queue.isEmpty Queue.empty == True

    Queue.isEmpty (Queue.fromList [ 1, 2 ]) == False

-}
isEmpty : Queue a -> Bool
isEmpty (Queue fl rl) =
    List.isEmpty fl && List.isEmpty rl


{-| Get size of `Queue`

    Queue.size Queue.empty == 0

    Queue.size (Queue.fromList [ 1, 2 ]) == 2

-}
size : Queue a -> Int
size (Queue fl rl) =
    List.length fl + List.length rl


{-| Add item to `Queue`

    Queue.size (Queue.enqueue 1 Queue.empty) == 1

    Queue.size (Queue.enqueue 1 (Queue.fromList [ 1, 2 ])) == 3

-}
enqueue : a -> Queue a -> Queue a
enqueue a (Queue fl rl) =
    queue fl <| a :: rl


{-| Take item from `Queue`

    Queue.dequeue Queue.empty == ( Nothing, Queue.empty )

    Queue.dequeue (Queue.fromList [ 1 ]) == ( Just 1, Queue.empty )

-}
dequeue : Queue a -> ( Maybe a, Queue a )
dequeue (Queue fl rl) =
    case fl of
        [] ->
            ( Nothing, Queue [] [] )

        head :: tail ->
            ( Just head, queue tail rl )


{-| Ask for front item without removing it from `Queue`

    Queue.front Queue.empty == Nothing

    Queue.front (Queue.fromList [ 1, 2 ]) == Just 1

-}
front : Queue a -> Maybe a
front (Queue fl _) =
    List.head fl


{-| Update value at the front of the queue

    Queue.toList (Queue.updateFront (Maybe.map (\x -> x + 1)) (Queue.singleton 3)) == [ 4 ]

    Queue.toList (Queue.updateFront (Maybe.map (\_ -> Just 42)) Queue.empty) == [ 42 ]

    Queue.toList (Queue.updateFront (Maybe.map (\_ -> Nothing)) (Queue.singleton 3)) == []

-}
updateFront : (Maybe a -> Maybe a) -> Queue a -> Queue a
updateFront f (Queue fl rl) =
    let
        update_ maybe t =
            case f maybe of
                Just a ->
                    a :: t

                Nothing ->
                    t
    in
    case fl of
        h :: t ->
            Queue (update_ (Just h) t) rl

        [] ->
            Queue (update_ Nothing []) rl



-- Lists


{-| Build `Queue` from `List`

    Queue.fromList [] == Queue.empty

    Queue.size (Queue.fromList [ 1, 2, 3 ]) == 3

-}
fromList : List a -> Queue a
fromList list =
    Queue list []


{-| Convert `Queue` to `List`

    Queue.toList (Queue.fromList []) == []

    Queue.toList (Queue.fromList [ 1, 2, 3 ]) == [ 1, 2, 3 ]

-}
toList : Queue a -> List a
toList (Queue fl rl) =
    fl ++ List.reverse rl



-- Transform


{-| Map function over `Queue`

    Queue.toList (Queue.map identity (Queue.fromList [ 1, 2 ])) == [ 1, 2 ]

    Queue.toList (Queue.map ((+) 1) (Queue.fromList [ 1, 2 ])) == [ 2, 3 ]

-}
map : (a -> b) -> Queue a -> Queue b
map fc (Queue fl rl) =
    let
        map_ =
            List.map fc
    in
    queue (map_ fl) (map_ rl)


{-| Filter items items in `Queue`

    Queue.toList (Queue.filter identity (Queue.fromList [ True, False ])) == [ True ]

    Queue.toList (Queue.filter ((<) 1) (Queue.fromList [ 1, 2 ])) == [ 2 ]

-}
filter : (a -> Bool) -> Queue a -> Queue a
filter fc (Queue fl rl) =
    let
        f =
            List.filter fc
    in
    queue (f fl) (f rl)
