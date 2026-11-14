module ElmMake.DependencyGraph exposing (Node, emit, order)

import Dict exposing (Dict)
import ElmMake.Errors as Errors
import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import Set exposing (Set)


type alias Node =
    { text : String, dependencies : List Global, isKernel : Bool, eager : Bool }


order : Set Global -> List Global
order globals =
    globals
        |> Set.toList
        |> List.sortBy
            (\( package, home, name ) ->
                if String.startsWith "Elm.Kernel." home then
                    ( "$", ( "elm", "kernel" ), String.dropLeft 11 home )

                else
                    ( name
                    , ( String.split "/" package |> List.head |> Maybe.withDefault ""
                      , String.split "/" package |> List.drop 1 |> List.head |> Maybe.withDefault ""
                      )
                    , home
                    )
            )


type alias Emission =
    { seen : Set Global, kernels : List String, declarations : List String }


{-| Mutually recursive Elm declarations are emitted together after their external
dependencies. Kernel cycles are distinct: a kernel block is marked before its
imports are visited and emitted after them.
-}
emit : Dict Global Node -> Global -> Result MakeError ( String, String )
emit nodes root =
    visit nodes root { seen = Set.empty, kernels = [], declarations = [] }
        |> Result.map (\state -> ( String.concat (List.reverse state.kernels), String.concat (List.reverse state.declarations) ))


visit : Dict Global Node -> Global -> Emission -> Result MakeError Emission
visit nodes global state =
    if Set.member global state.seen then
        Ok state

    else
        case Dict.get global nodes of
            Nothing ->
                Err (Errors.internal MissingGraphNode (globalName global))

            Just node ->
                let
                    members =
                        if node.isKernel then
                            Set.singleton global

                        else
                            component nodes global

                    group =
                        order members |> List.filterMap (\name -> Dict.get name nodes)

                    dependencies =
                        (if Set.size members == 1 then
                            node.dependencies

                         else
                            List.concatMap .dependencies group |> Set.fromList |> order
                        )
                            |> List.filter (\dependency -> not (Set.member dependency members))

                    next =
                        { state | seen = Set.union state.seen members }

                    recursive =
                        Set.size members > 1 || List.member global node.dependencies
                in
                if not node.isKernel && recursive && List.any .eager group then
                    Err (Errors.unsupported Location.Project RecursiveValueInitialization)

                else
                    foldResult (visit nodes) next dependencies
                        |> Result.map
                            (\result ->
                                if node.isKernel then
                                    { result | kernels = node.text :: result.kernels }

                                else
                                    { result | declarations = String.concat (List.map .text group) :: result.declarations }
                            )


component : Dict Global Node -> Global -> Set Global
component nodes (( package, home, _ ) as root) =
    let
        local (( owner, moduleName, _ ) as target) =
            owner == package && moduleName == home && Dict.member target nodes

        reachable start =
            let
                loop pending seen =
                    case pending of
                        [] ->
                            seen

                        current :: rest ->
                            if Set.member current seen then
                                loop rest seen

                            else
                                let
                                    dependencies =
                                        Dict.get current nodes |> Maybe.map (.dependencies >> List.filter local) |> Maybe.withDefault []
                                in
                                loop (dependencies ++ rest) (Set.insert current seen)
            in
            loop [ start ] Set.empty
    in
    reachable root |> Set.filter (\candidate -> Set.member root (reachable candidate))
