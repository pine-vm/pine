module ElmMake.EmissionMode exposing (field, prepare, tag)

import Dict exposing (Dict)
import ElmMake.JavaScriptLiterals as Literals
import ElmMake.JavaScriptNames as Names
import ElmMake.JavaScriptSyntax as JS
import ElmMake.Kernel as Kernel
import ElmMake.Model exposing (..)
import ElmSyntax.Abstract.Declaration exposing (Declaration(..))
import ElmSyntax.Abstract.Expression exposing (Expression(..), LetDeclaration(..))
import ElmSyntax.Abstract.Pattern exposing (Pattern(..))
import ElmSyntax.Abstract.TypeAnnotation as Type
import Set


field : Mode -> String -> String
field mode name =
    Dict.get name mode.fields |> Maybe.withDefault (Names.local name)


tag : Mode -> Global -> Int -> JS.Expr
tag mode ( package, home, name ) index =
    if mode.flavor /= Optimize then
        JS.Literal (Literals.string name)

    else
        JS.Literal
            (String.fromInt
                (if package == "elm/core" && home == "Dict" && List.member name [ "RBNode_elm_builtin", "RBEmpty_elm_builtin" ] then
                    -(index + 1)

                 else
                    index
                )
            )


{-| Counts accumulate within a module, but classic object graphs combine their
field maps with left-biased union, not addition. Application modules precede
package artifacts; canonical package/module ordering decides other collisions.
Dead declarations in those modules still contribute.
-}
prepare : Flavor -> Environment -> List Module -> Mode
prepare flavor environment compiledModules =
    if flavor /= Optimize then
        { flavor = flavor, fields = Dict.empty }

    else
        let
            included =
                Set.fromList (List.map (\module_ -> ( module_.package, module_.name )) compiledModules)

            sourceCounts =
                Dict.values environment.modules
                    |> List.filter (\module_ -> module_.package /= "author/project" || Set.member ( module_.package, module_.name ) included)
                    |> List.map (\module_ -> ( ( module_.package, module_.name ), List.foldl declaration Dict.empty module_.syntax.declarations ))
                    |> Dict.fromList

            artifacts =
                Dict.union sourceCounts (Dict.map (\_ source -> kernelFields source Dict.empty) environment.kernels)

            combine values =
                Dict.toList values
                    |> List.sortBy (\( ( package, home ), _ ) -> ( String.split "/" package, home ))
                    |> List.map Tuple.second
                    |> List.foldr Dict.union Dict.empty

            counts =
                Dict.union
                    (Dict.filter (\( package, _ ) _ -> package == "author/project") artifacts |> combine)
                    (Dict.filter (\( package, _ ) _ -> package /= "author/project") artifacts |> combine)

            ordered =
                Dict.toList counts |> List.sortBy (\( name, count ) -> ( -count, name ))
        in
        { flavor = flavor
        , fields = ordered |> List.indexedMap (\index ( name, _ ) -> ( name, Kernel.shortName index )) |> Dict.fromList
        }


add : List String -> Dict String Int -> Dict String Int
add names counts =
    List.foldl (\name -> Dict.update name (\count -> Just (1 + Maybe.withDefault 0 count))) counts names


declaration : Declaration -> Dict String Int -> Dict String Int
declaration value counts =
    case value of
        FunctionDeclaration function ->
            expression function.declaration.expression (List.foldl pattern counts function.declaration.arguments)

        AliasDeclaration alias ->
            case alias.typeAnnotation of
                Type.Record fields ->
                    add (List.map .fieldName fields) counts

                _ ->
                    counts

        _ ->
            counts


pattern : Pattern -> Dict String Int -> Dict String Int
pattern value counts =
    case value of
        RecordPattern fields ->
            add fields counts

        TuplePattern items ->
            List.foldl pattern counts items

        ListPattern items ->
            List.foldl pattern counts items

        NamedPattern _ items ->
            List.foldl pattern counts items

        UnConsPattern head tail ->
            pattern tail (pattern head counts)

        AsPattern inner _ ->
            pattern inner counts

        _ ->
            counts


expression : Expression -> Dict String Int -> Dict String Int
expression value counts =
    let
        children items =
            List.foldl expression counts items
    in
    case value of
        Application function arguments ->
            children (function :: arguments)

        OperatorApplication _ _ left right ->
            children [ left, right ]

        Negation inner ->
            expression inner counts

        IfBlock condition yes no ->
            children [ condition, yes, no ]

        ListExpr items ->
            children items

        TupledExpression items ->
            children items

        RecordExpr fields ->
            List.foldl expression (add (List.map .fieldName fields) counts) (List.map .value fields)

        RecordUpdateExpression _ fields ->
            List.foldl expression (add (List.map .fieldName fields) counts) (List.map .value fields)

        RecordAccess inner name ->
            expression inner (add [ name ] counts)

        RecordAccessFunction name ->
            add [ name ] counts

        LambdaExpression parameters body ->
            expression body (List.foldl pattern counts parameters)

        CaseExpression subject cases ->
            List.foldl (\branch frequencies -> expression branch.expression (pattern branch.pattern frequencies)) (expression subject counts) cases

        LetExpression definitions body ->
            expression body
                (List.foldl
                    (\definition frequencies ->
                        case definition of
                            LetFunction function ->
                                expression function.declaration.expression (List.foldl pattern frequencies function.declaration.arguments)

                            LetDestructuring target inner ->
                                expression inner (pattern target frequencies)
                    )
                    counts
                    definitions
                )

        _ ->
            counts


kernelFields : String -> Dict String Int -> Dict String Int
kernelFields source counts =
    case String.indexes "__$" source |> List.head of
        Nothing ->
            counts

        Just offset ->
            let
                after =
                    String.dropLeft (offset + 3) source

                name =
                    String.left (Kernel.tokenLength 0 after) after
            in
            kernelFields (String.dropLeft (String.length name) after) (add [ name ] counts)
