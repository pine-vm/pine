module ElmMake.CyclicDeclarations exposing (allocate)

import Dict exposing (Dict)
import ElmMake.Canonicalize as Canonicalize
import ElmMake.DependencyGraph as Graph
import ElmMake.Errors as Errors
import ElmMake.JavaScript as JavaScript
import ElmMake.JavaScriptSyntax as JS
import ElmMake.LocalDeclarations as LocalDeclarations
import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import ElmMake.StronglyConnected as StronglyConnected
import Set exposing (Set)


{-| Recursive components share the classic fresh-name supply. Cyclic values use
named thunks until initialization succeeds, then each thunk returns its cached
value. References outside the component keep the ordinary initialized binding.
-}
allocate : Mode -> Environment -> Dict Global Graph.Node -> Result MakeError (Dict Global Graph.Node)
allocate mode environment nodes =
    let
        graph =
            nodes
                |> Dict.filter (\_ node -> not node.isKernel)
                |> Dict.map
                    (\( package, home, _ ) node ->
                        List.filter (\( owner, module_, _ ) -> owner == package && module_ == home) node.dependencies
                    )

        visit members current =
            if List.length members <= 1 then
                Ok current

            else
                let
                    ordered =
                        Graph.order (Set.fromList members)

                    values =
                        List.filter (\global -> Dict.get global current |> Maybe.map .eager |> Maybe.withDefault False) ordered |> Set.fromList
                in
                if Set.size values == List.length members then
                    Err (Errors.unsupported Location.Project RecursiveValueInitialization)

                else
                    compilationOrder environment ordered
                        |> Result.andThen
                            (\order ->
                                foldResult (compileMember mode environment values) { next = 0, declarations = Dict.empty } order
                                    |> Result.map (Tuple.pair order)
                            )
                        |> Result.map
                            (\( order, compiled ) ->
                                let
                                    functions =
                                        List.filter (\global -> not (Set.member global values)) ordered

                                    valueOrder =
                                        List.reverse (Set.toList values)

                                    text =
                                        String.concat (List.filterMap (\global -> Dict.get global compiled.declarations) (functions ++ valueOrder))
                                            ++ (if Set.isEmpty values then
                                                    ""

                                                else
                                                    initialization (List.reverse order) valueOrder
                                               )

                                    representative =
                                        List.head ordered
                                in
                                List.foldl
                                    (\global ->
                                        Dict.update global
                                            (Maybe.map
                                                (\node ->
                                                    { node
                                                        | text =
                                                            if Just global == representative then
                                                                text

                                                            else
                                                                ""
                                                        , eager = False
                                                    }
                                                )
                                            )
                                    )
                                    current
                                    members
                            )
    in
    foldResult visit nodes (StronglyConnected.dependencyOrder graph)


compilationOrder : Environment -> List Global -> Result MakeError (List Global)
compilationOrder environment members =
    let
        dependencies global =
            memberDefinition environment global
                |> Result.andThen
                    (\( module_, definition ) ->
                        let
                            scope =
                                Canonicalize.interfaceScope environment module_
                        in
                        case definition of
                            Function [] body ->
                                LocalDeclarations.eagerReferences body
                                    |> Set.toList
                                    |> traverse (\( qualifier, name ) -> Canonicalize.resolve scope qualifier name)

                            _ ->
                                Ok []
                    )
                |> Result.map (\found -> ( global, List.filter (\target -> List.member target members) found ))
    in
    traverse dependencies members
        |> Result.map (Dict.fromList >> StronglyConnected.dependencyOrder >> List.concat)


memberDefinition : Environment -> Global -> Result MakeError ( Module, Definition )
memberDefinition environment ( package, home, name ) =
    Dict.get ( package, home ) environment.modules
        |> Result.fromMaybe (Errors.internal CompilerInvariant ("Missing recursive module " ++ home))
        |> Result.andThen
            (\module_ ->
                Canonicalize.definition module_ name
                    |> Result.fromMaybe (Errors.internal CompilerInvariant ("Missing recursive declaration " ++ name))
                    |> Result.map (Tuple.pair module_)
            )


compileMember : Mode -> Environment -> Set Global -> Global -> { next : Int, declarations : Dict Global String } -> Result MakeError { next : Int, declarations : Dict Global String }
compileMember mode environment values (( _, _, name ) as global) state =
    memberDefinition environment global
        |> Result.andThen
            (\( module_, definition ) ->
                let
                    context =
                        Canonicalize.definitionScope environment module_ name

                    scope =
                        { context | mode = mode, cyclicValues = values }

                    compiled =
                        case definition of
                            Function [] body ->
                                JavaScript.compileCyclicValueFrom state.next scope name body

                            _ ->
                                JavaScript.compileDefinitionFrom state.next scope name definition
                in
                compiled
                    |> Result.map
                        (\result ->
                            { next = result.next, declarations = Dict.insert global result.code.text state.declarations }
                        )
            )


initialization : List Global -> List Global -> String
initialization members values =
    let
        bindings global =
            [ JS.Var (globalName global) (JS.Call (JS.Ref (cyclicName global)) [])
            , JS.Assign (cyclicName global) (JS.Function [] [ JS.Return (JS.Ref (globalName global)) ])
            ]

        home =
            List.head members |> Maybe.map (\( _, module_, _ ) -> module_) |> Maybe.withDefault ""

        names =
            List.map (\( _, _, name ) -> name) members

        message =
            "Some top-level definitions from `"
                ++ home
                ++ "` are causing infinite recursion:\n\n  \u{250C}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2510}\n"
                ++ String.join "  \u{2502}     \u{2193}\n" (List.map (\name -> "  \u{2502}    " ++ name ++ "\n") names)
                ++ "  \u{2514}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2518}\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!"
    in
    JS.renderStatement (JS.TryCatch (List.concatMap bindings values) "$" [ JS.ThrowValue (JS.Literal (JavaScript.quote message)) ])
