module ElmMake.PatternDecisions exposing (compile)

import Dict exposing (Dict)
import ElmMake.Errors as Errors
import ElmMake.JavaScriptSyntax as JS exposing (Expr(..), Stmt(..))
import ElmMake.Model exposing (MakeError(..), UnsupportedCapability(..), traverse)
import ElmMake.Patterns as Patterns exposing (Test(..))
import ElmMake.SourceLocation as Location


type alias Row =
    { target : Int, tests : List Test, body : List Stmt }


type Tree
    = Leaf Int (List Stmt)
    | Chain (List Test) Tree Tree
    | FanOut Test (List ( Test, Tree )) Tree
    | NoMatch


{-| Select paths by fewest default branches, then smallest branching factor.
Repeated targets become labelled exits rather than duplicated branch bodies.
-}
compile : Location.Location -> String -> List { tests : List Test, body : List Stmt } -> Result MakeError (List Stmt)
compile where_ label branches =
    let
        tree =
            branches |> List.indexedMap (\index branch -> { target = index, tests = branch.tests, body = branch.body }) |> build

        counts =
            targets tree Dict.empty

        repeated =
            Dict.toList counts |> List.filter (\( _, branch ) -> branch.count > 1)

        render node =
            case node of
                Leaf target body ->
                    if List.any (\( index, _ ) -> target == index) repeated then
                        Ok [ Break (label ++ "$" ++ String.fromInt target) ]

                    else
                        Ok body

                Chain tests yes no ->
                    Result.map2
                        (\success fallback ->
                            case List.map Patterns.testExpression tests of
                                first :: rest ->
                                    [ If (List.foldl (\next left -> Binary "&&" left next) first rest) success fallback ]

                                [] ->
                                    success
                        )
                        (render yes)
                        (render no)

                FanOut test edges fallback ->
                    Result.map2
                        (\cases default -> [ Switch (Patterns.subject test) (cases ++ [ ( Nothing, default ) ]) ])
                        (traverse (\( edge, child ) -> render child |> Result.map (Tuple.pair (Just (Patterns.tagValue edge)))) edges)
                        (render fallback)

                NoMatch ->
                    Err (Errors.unsupported where_ IncompleteCase)
    in
    render tree
        |> Result.map
            (\statements ->
                List.foldr
                    (\( index, branch ) nested -> Loop (label ++ "$" ++ String.fromInt index) nested :: branch.body)
                    statements
                    repeated
            )


build : List Row -> Tree
build rows =
    case rows of
        [] ->
            NoMatch

        first :: _ ->
            if List.isEmpty first.tests then
                Leaf first.target first.body

            else
                case pick rows of
                    Nothing ->
                        NoMatch

                    Just selected ->
                        let
                            edges =
                                relevant selected rows

                            complete =
                                Patterns.cardinality selected > 0 && List.length edges == Patterns.cardinality selected

                            fallbackRows =
                                List.filter (\row -> not (List.any (Patterns.sameSubject selected) row.tests)) rows

                            compiled =
                                List.map (\edge -> ( edge, build (forEdge edge rows) )) edges
                        in
                        case ( compiled, complete, fallbackRows ) of
                            ( [ ( _, child ) ], True, _ ) ->
                                child

                            ( [ ( test, yes ), ( _, no ) ], True, _ ) ->
                                chain test yes no

                            ( [ ( test, yes ) ], False, _ ) ->
                                chain test yes (build fallbackRows)

                            ( [], _, _ ) ->
                                build fallbackRows

                            _ ->
                                if complete then
                                    case List.reverse compiled of
                                        ( _, last ) :: rest ->
                                            FanOut selected (List.reverse rest) last

                                        [] ->
                                            NoMatch

                                else
                                    FanOut selected compiled (build fallbackRows)


chain : Test -> Tree -> Tree -> Tree
chain test yes no =
    case yes of
        Chain tests success failure ->
            if failure == no then
                Chain (test :: tests) success no

            else
                Chain [ test ] yes no

        _ ->
            Chain [ test ] yes no


forEdge : Test -> List Row -> List Row
forEdge edge =
    List.filterMap
        (\row ->
            case List.filter (Patterns.sameSubject edge) row.tests |> List.head of
                Nothing ->
                    Just row

                Just candidate ->
                    if candidate == edge then
                        Just { row | tests = List.filter ((/=) edge) row.tests }

                    else
                        Nothing
        )


relevant : Test -> List Row -> List Test
relevant selected rows =
    rows
        |> List.filterMap (\row -> List.filter (Patterns.sameSubject selected) row.tests |> List.head)
        |> List.foldr
            (\test unique ->
                if List.member test unique then
                    unique

                else
                    test :: unique
            )
            []


pick : List Row -> Maybe Test
pick rows =
    let
        available row =
            row.tests
                |> List.filter
                    (\test -> not (List.any (\parent -> below (value parent) (value test)) row.tests))

        choices =
            List.concatMap available rows

        defaults test =
            List.length (List.filter (\row -> not (List.any (Patterns.sameSubject test) row.tests)) rows)

        branching test =
            let
                edges =
                    relevant test rows
            in
            List.length edges
                + (if Patterns.cardinality test == List.length edges then
                    0

                   else
                    1
                  )
    in
    bests defaults choices |> bests branching |> List.head


bests : (a -> Int) -> List a -> List a
bests weight items =
    case items of
        [] ->
            []

        first :: rest ->
            List.foldl
                (\item state ->
                    let
                        score =
                            weight item
                    in
                    if score < state.minimum then
                        { minimum = score, items = [ item ] }

                    else if score == state.minimum then
                        { state | items = item :: state.items }

                    else
                        state
                )
                { minimum = weight first, items = [ first ] }
                rest
                |> .items


value : Test -> Expr
value test =
    case test of
        Tag root _ _ ->
            root

        IsNil root ->
            root

        IsCons root ->
            root

        IsValue root _ ->
            root

        IsChar root _ _ ->
            root

        IsBool root _ ->
            root


below : Expr -> Expr -> Bool
below ancestor child =
    case child of
        Access parent _ ->
            parent == ancestor || below ancestor parent

        _ ->
            False


targets : Tree -> Dict Int { count : Int, body : List Stmt } -> Dict Int { count : Int, body : List Stmt }
targets tree counts =
    case tree of
        Leaf index body ->
            Dict.update index
                (\previous -> Just { count = 1 + (previous |> Maybe.map .count |> Maybe.withDefault 0), body = body })
                counts

        Chain _ yes no ->
            targets no (targets yes counts)

        FanOut _ edges fallback ->
            List.foldl (\( _, child ) totals -> targets child totals) (targets fallback counts) edges

        NoMatch ->
            counts
