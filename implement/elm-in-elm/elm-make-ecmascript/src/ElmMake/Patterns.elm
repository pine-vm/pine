module ElmMake.Patterns exposing (Match, Test(..), bindings, cardinality, names, sameSubject, subject, tagValue, testExpression)

import Dict exposing (Dict)
import ElmMake.Canonicalize as Canonicalize
import ElmMake.CoreCalls as CoreCalls
import ElmMake.EmissionMode as EmissionMode
import ElmMake.Errors as Errors
import ElmMake.JavaScriptLiterals as Literals
import ElmMake.JavaScriptNames as Names
import ElmMake.JavaScriptSyntax as JS exposing (Expr(..), Stmt(..))
import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import ElmSyntax.Abstract.Declaration exposing (Declaration(..))
import ElmSyntax.Abstract.Pattern exposing (Pattern(..))


type Test
    = Tag Expr Expr Int
    | IsNil Expr
    | IsCons Expr
    | IsValue Expr Expr
    | IsChar Expr Expr Bool
    | IsBool Expr Bool


type alias Match =
    { variables : List ( String, Expr )
    , tests : List Test
    , next : Int
    }


names : Pattern -> List String
names pattern =
    case pattern of
        VarPattern name ->
            [ name ]

        AsPattern inner name ->
            name :: names inner

        RecordPattern fields ->
            fields

        TuplePattern items ->
            List.concatMap names items

        ListPattern items ->
            List.concatMap names items

        UnConsPattern head tail ->
            names head ++ names tail

        NamedPattern _ items ->
            List.concatMap names items

        _ ->
            []


bindings : Scope -> Pattern -> Expr -> Int -> Result MakeError Match
bindings scope pattern root next =
    bindingsHelp scope pattern root root next


bindingsHelp : Scope -> Pattern -> Expr -> Expr -> Int -> Result MakeError Match
bindingsHelp scope pattern root testRoot next =
    let
        empty tests =
            Ok { variables = [], tests = tests, next = next }

        children patterns =
            foldResult
                (\( index, child ) result ->
                    bindingsHelp scope child (Access root (Names.field index)) (Access testRoot (Names.field index)) result.next
                        |> Result.map
                            (\match ->
                                { variables = result.variables ++ match.variables
                                , tests = result.tests ++ match.tests
                                , next = match.next
                                }
                            )
                )
                { variables = [], tests = [], next = next }
                (List.indexedMap Tuple.pair patterns)

        withRoot patterns =
            case root of
                Ref _ ->
                    children patterns

                _ ->
                    let
                        temporary =
                            "_v" ++ String.fromInt next
                    in
                    bindingsHelp scope (TuplePattern patterns) (Ref temporary) testRoot (next + 1)
                        |> Result.map (\match -> { match | variables = ( temporary, root ) :: match.variables })
    in
    case pattern of
        AllPattern ->
            empty []

        UnitPattern ->
            empty []

        VarPattern name ->
            Ok
                { variables =
                    if root == Ref (Names.local name) then
                        []

                    else
                        [ ( Names.local name, root ) ]
                , tests = []
                , next = next
                }

        AsPattern inner name ->
            bindingsHelp scope inner (Ref (Names.local name)) testRoot next
                |> Result.map
                    (\match ->
                        { match
                            | variables = (if root == Ref (Names.local name) then
                                            []

                                           else
                                            [ ( Names.local name, root ) ]
                                          )
                                            ++ match.variables
                        }
                    )

        TuplePattern patterns ->
            withRoot patterns

        RecordPattern fields ->
            Ok { variables = List.map (\field -> ( Names.local field, Access root (EmissionMode.field scope.mode field) )) fields, tests = [], next = next }

        NamedPattern name patterns ->
            Canonicalize.resolve scope name.moduleName name.name
                |> Result.andThen
                    (\( package, home, _ ) ->
                        let
                            info =
                                Canonicalize.constructorInfo scope.environment ( package, home, name.name )

                            choices =
                                Dict.get ( package, home ) scope.environment.modules
                                    |> Maybe.map (.syntax >> .declarations)
                                    |> Maybe.withDefault []
                                    |> List.filterMap
                                        (\declaration ->
                                            case declaration of
                                                ChoiceTypeDeclaration choice ->
                                                    if List.any (\ctor -> ctor.name == name.name) choice.constructors then
                                                        Just (List.length choice.constructors)

                                                    else
                                                        Nothing

                                                _ ->
                                                    Nothing
                                        )

                            test =
                                if ( package, home, name.name ) == ( "elm/core", "Basics", "True" ) then
                                    [ IsBool testRoot True ]

                                else if ( package, home, name.name ) == ( "elm/core", "Basics", "False" ) then
                                    [ IsBool testRoot False ]

                                else
                                    case choices of
                                        [ 1 ] ->
                                            []

                                        [ count ] ->
                                            let
                                                constructor =
                                                    info |> Maybe.map .index |> Maybe.withDefault 0

                                                tag =
                                                    EmissionMode.tag scope.mode ( package, home, name.name ) constructor

                                                target =
                                                    if scope.mode.flavor == Optimize && (info |> Maybe.map .enumeration |> Maybe.withDefault False) then
                                                        testRoot

                                                    else
                                                        Access testRoot "$"
                                            in
                                            [ Tag target tag count ]

                                        _ ->
                                            [ Tag (Access testRoot "$") (Literal (Literals.string name.name)) 0 ]
                        in
                        (if ( package, home ) == ( "elm/core", "Basics" ) && List.member name.name [ "True", "False" ] then
                            empty []

                         else if scope.mode.flavor == Optimize && (info |> Maybe.map (\details -> details.arity == 1 && details.alternatives == 1) |> Maybe.withDefault False) then
                            case patterns of
                                [ inner ] ->
                                    bindingsHelp scope inner root testRoot next

                                _ ->
                                    children patterns

                         else if List.length patterns == 1 then
                            children patterns

                         else
                            withRoot patterns
                        )
                            |> Result.map (\match -> { match | tests = test ++ match.tests })
                    )

        ListPattern [] ->
            empty [ IsNil testRoot ]

        ListPattern (head :: tail) ->
            bindingsHelp scope (UnConsPattern head (ListPattern tail)) root testRoot next

        UnConsPattern head tail ->
            withRoot [ head, tail ] |> Result.map (\match -> { match | tests = IsCons testRoot :: match.tests })

        StringPattern text ->
            empty [ IsValue testRoot (Literal (Literals.string text)) ]

        IntPattern number ->
            empty [ IsValue testRoot (Literal (String.fromInt number)) ]

        CharPattern code ->
            empty
                [ IsChar testRoot (Literal (Literals.characterPattern code)) (scope.mode.flavor == Optimize) ]

        FloatPattern _ ->
            Err (Errors.unsupported (Location.File scope.module_.file) FloatPatterns)


testExpression : Test -> Expr
testExpression test =
    case test of
        Tag value tag _ ->
            CoreCalls.compile "Basics" "eq" [ value, tag ]

        IsNil value ->
            Unary "!" (Access value "b")

        IsCons value ->
            Access value "b"

        IsValue value literal ->
            Binary "===" value literal

        IsChar value literal unboxed ->
            Binary "==="
                literal
                (if unboxed then
                    value

                 else
                    Call (Access value "valueOf") []
                )

        IsBool value True ->
            value

        IsBool value False ->
            Unary "!" value


sameSubject : Test -> Test -> Bool
sameSubject left right =
    case ( left, right ) of
        ( Tag a _ _, Tag b _ _ ) ->
            a == b

        ( IsNil a, IsNil b ) ->
            a == b

        ( IsCons a, IsCons b ) ->
            a == b

        ( IsNil a, IsCons b ) ->
            a == b

        ( IsCons a, IsNil b ) ->
            a == b

        ( IsValue a _, IsValue b _ ) ->
            a == b

        ( IsChar a _ _, IsChar b _ _ ) ->
            a == b

        ( IsBool a _, IsBool b _ ) ->
            a == b

        _ ->
            False


cardinality : Test -> Int
cardinality test =
    case test of
        Tag _ _ count ->
            count

        IsNil _ ->
            2

        IsCons _ ->
            2

        IsBool _ _ ->
            2

        IsValue _ _ ->
            0

        IsChar _ _ _ ->
            0


subject : Test -> Expr
subject test =
    case test of
        Tag value _ _ ->
            value

        IsValue value _ ->
            value

        IsChar value _ unboxed ->
            if unboxed then
                value

            else
                Call (Access value "valueOf") []

        _ ->
            testExpression test


tagValue : Test -> Expr
tagValue test =
    case test of
        Tag _ name _ ->
            name

        IsValue _ literal ->
            literal

        IsChar _ literal _ ->
            literal

        _ ->
            testExpression test
