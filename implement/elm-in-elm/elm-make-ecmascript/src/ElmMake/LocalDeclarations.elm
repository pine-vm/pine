module ElmMake.LocalDeclarations exposing (IndexedDeclaration, eagerReferences, isSelfRecursive, order, references)

import Dict
import ElmMake.Errors as Errors
import ElmMake.Model exposing (MakeError(..), UnsupportedCapability(..))
import ElmMake.Patterns as Patterns
import ElmMake.SourceLocation as Location
import ElmMake.StronglyConnected as StronglyConnected
import ElmSyntax.Abstract.Expression exposing (Expression(..), LetDeclaration(..))
import ElmSyntax.Abstract.Pattern
import Set exposing (Set)


{-| Named bindings and destructuring edges use the same dependency graph.
The synthetic destructuring node ensures all of a pattern's bindings share one
initializer and gives dependency sorting the same keys as canonical Elm.
-}
type alias IndexedDeclaration =
    { index : Int, declaration : LetDeclaration }


isSelfRecursive : LetDeclaration -> Bool
isSelfRecursive declaration =
    case declaration of
        LetFunction function ->
            Set.member function.declaration.name (free function.declaration.expression)

        LetDestructuring _ _ ->
            False


order : Location.Location -> List LetDeclaration -> Result MakeError (List IndexedDeclaration)
order where_ declarations =
    let
        nodes =
            declarations
                |> List.indexedMap (\index declaration -> { index = index, declaration = declaration })
                |> List.concatMap
                    (\indexed ->
                        let
                            declaration =
                                indexed.declaration
                        in
                        case declaration of
                            LetFunction function ->
                                [ ( function.declaration.name
                                  , { declaration = Just indexed
                                    , dependencies = Set.diff (free function.declaration.expression) (Set.fromList (List.concatMap Patterns.names function.declaration.arguments)) |> Set.toList
                                    }
                                  )
                                ]

                            LetDestructuring pattern value ->
                                let
                                    bound =
                                        patternKeyNames pattern []

                                    key =
                                        "_M$" ++ (List.head bound |> Maybe.withDefault "")
                                in
                                ( key, { declaration = Just indexed, dependencies = free value |> Set.toList } )
                                    :: List.map (\name -> ( name, { declaration = Nothing, dependencies = [ key ] } )) bound
                    )
                |> Dict.fromList

        graph =
            Dict.map (\_ node -> List.filter (\key -> Dict.member key nodes) node.dependencies) nodes

        component keys =
            let
                members =
                    List.filterMap (\key -> Dict.get key nodes |> Maybe.andThen .declaration) keys

                recursive =
                    List.length keys > 1 || List.any (\key -> Dict.get key graph |> Maybe.withDefault [] |> List.member key) keys

                functionsOnly =
                    List.all
                        (\indexed ->
                            case indexed.declaration of
                                LetFunction function ->
                                    not (List.isEmpty function.declaration.arguments)

                                LetDestructuring _ _ ->
                                    False
                        )
                        members
            in
            if recursive && (not functionsOnly || List.length members /= List.length keys) then
                Err (Errors.unsupported where_ RecursiveValueInitialization)

            else
                Ok members
    in
    StronglyConnected.dependencyOrder graph
        |> ElmMake.Model.traverse component
        |> Result.map List.concat


patternKeyNames : ElmSyntax.Abstract.Pattern.Pattern -> List String -> List String
patternKeyNames pattern collected =
    case pattern of
        ElmSyntax.Abstract.Pattern.VarPattern name ->
            name :: collected

        ElmSyntax.Abstract.Pattern.RecordPattern fields ->
            List.reverse fields ++ collected

        ElmSyntax.Abstract.Pattern.AsPattern inner name ->
            patternKeyNames inner (name :: collected)

        ElmSyntax.Abstract.Pattern.TuplePattern items ->
            List.foldl patternKeyNames collected items

        ElmSyntax.Abstract.Pattern.ListPattern items ->
            List.foldl patternKeyNames collected items

        ElmSyntax.Abstract.Pattern.NamedPattern _ items ->
            List.foldl patternKeyNames collected items

        ElmSyntax.Abstract.Pattern.UnConsPattern head tail ->
            patternKeyNames tail (patternKeyNames head collected)

        _ ->
            collected


names : LetDeclaration -> List String
names declaration =
    case declaration of
        LetFunction function ->
            [ function.declaration.name ]

        LetDestructuring pattern _ ->
            Patterns.names pattern


free : Expression -> Set String
free expression =
    references expression
        |> Set.toList
        |> List.filterMap
            (\( qualifier, name ) ->
                if List.isEmpty qualifier then
                    Just name

                else
                    Nothing
            )
        |> Set.fromList


references : Expression -> Set ( List String, String )
references =
    collectReferences True


eagerReferences : Expression -> Set ( List String, String )
eagerReferences =
    collectReferences False


collectReferences : Bool -> Expression -> Set ( List String, String )
collectReferences includeDelayed expression =
    let
        references_ =
            collectReferences includeDelayed

        children values =
            List.foldl (Set.union << references_) Set.empty values

        without bound found =
            Set.diff found (Set.fromList (List.map (Tuple.pair []) bound))
    in
    case expression of
        Identifier qualifier name ->
            Set.singleton ( qualifier, name )

        Application function arguments ->
            children (function :: arguments)

        OperatorApplication _ _ left right ->
            children [ left, right ]

        Negation value ->
            references_ value

        ListExpr items ->
            children items

        TupledExpression items ->
            children items

        IfBlock condition yes no ->
            children [ condition, yes, no ]

        RecordAccess target _ ->
            references_ target

        RecordUpdateExpression name fields ->
            Set.insert ( [], name ) (children (List.map .value fields))

        RecordExpr fields ->
            children (List.map .value fields)

        LambdaExpression parameters result ->
            if includeDelayed then
                without (List.concatMap Patterns.names parameters) (references_ result)

            else
                Set.empty

        CaseExpression subject cases ->
            Set.union (references_ subject)
                (List.foldl
                    (\case_ refs -> Set.union refs (without (Patterns.names case_.pattern) (references_ case_.expression)))
                    Set.empty
                    cases
                )

        LetExpression declarations result ->
            without (List.concatMap names declarations)
                (Set.union (references_ result)
                    (List.foldl
                        (\declaration refs ->
                            let
                                own =
                                    case declaration of
                                        LetFunction function ->
                                            if includeDelayed || List.isEmpty function.declaration.arguments then
                                                without (List.concatMap Patterns.names function.declaration.arguments) (references_ function.declaration.expression)

                                            else
                                                Set.empty

                                        LetDestructuring _ value ->
                                            references_ value
                            in
                            Set.union refs own
                        )
                        Set.empty
                        declarations
                    )
                )

        _ ->
            Set.empty
