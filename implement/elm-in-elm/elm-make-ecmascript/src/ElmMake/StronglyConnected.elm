module ElmMake.StronglyConnected exposing (dependencyOrder)

import Dict exposing (Dict)
import Set exposing (Set)


{-| Dependency-first strongly connected components. Vertex keys and adjacency
order are explicit, so generated binding order is stable across source edits.
-}
dependencyOrder : Dict comparable (List comparable) -> List (List comparable)
dependencyOrder graph =
    let
        transposed =
            Dict.foldl
                (\from dependencies reversed ->
                    List.foldl
                        (\target current ->
                            if Dict.member target graph then
                                Dict.update target (Maybe.map ((::) from)) current

                            else
                                current
                        )
                        reversed
                        dependencies
                )
                (Dict.map (\_ _ -> []) graph)
                graph

        finish current state =
            if Set.member current state.seen then
                state

            else
                let
                    next =
                        List.foldl finish { state | seen = Set.insert current state.seen } (Dict.get current transposed |> Maybe.withDefault [])
                in
                { next | order = current :: next.order }

        roots =
            List.foldl finish { seen = Set.empty, order = [] } (Dict.keys graph) |> .order

        gather current state =
            if Set.member current state.seen then
                state

            else
                List.foldl gather
                    { seen = Set.insert current state.seen, component = state.component ++ [ current ] }
                    (Dict.get current graph |> Maybe.withDefault [] |> List.filter (\key -> Dict.member key graph))

        collect root state =
            if Set.member root state.seen then
                state

            else
                let
                    next =
                        gather root { seen = state.seen, component = [] }
                in
                { seen = next.seen, components = state.components ++ [ next.component ] }
    in
    List.foldl collect { seen = Set.empty, components = [] } roots |> .components
