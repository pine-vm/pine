module ElmMake.Types exposing (Alias, Type(..), alias, expandAlias, resolve)

import Dict exposing (Dict)
import ElmMake.Canonicalize as Canonicalize
import ElmMake.Errors as Errors
import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import ElmSyntax.Abstract.Declaration exposing (Declaration(..))
import ElmSyntax.Abstract.TypeAnnotation as Syntax


{-| Resolve type identities before substituting aliases. An argument keeps its
original module even when substituted into an alias declared in another module.
-}
type Type
    = Unit
    | GenericType String
    | Typed Global (List Type)
    | Tupled (List Type)
    | Record (Maybe String) (List ( String, Type ))
    | FunctionTypeAnnotation Type Type


type alias Alias =
    { args : List String, body : Type }


resolve : Environment -> Module -> Syntax.TypeAnnotation -> Result MakeError Type
resolve environment owner annotation =
    let
        child =
            resolve environment owner

        record extension fields =
            traverse (\field -> child field.fieldType |> Result.map (Tuple.pair field.fieldName)) fields
                |> Result.map (Record extension)
    in
    case annotation of
        Syntax.Unit ->
            Ok Unit

        Syntax.GenericType name ->
            Ok (GenericType name)

        Syntax.Typed qualifier name arguments ->
            Result.map2
                (\module_ types -> Typed ( module_.package, module_.name, name ) types)
                (Canonicalize.resolveType (Canonicalize.interfaceScope environment owner) qualifier name)
                (traverse child arguments)

        Syntax.Tupled [ item ] ->
            child item

        Syntax.Tupled items ->
            traverse child items |> Result.map Tupled

        Syntax.Record fields ->
            record Nothing fields

        Syntax.GenericRecord name fields ->
            record (Just name) fields

        Syntax.FunctionTypeAnnotation argument result ->
            Result.map2 FunctionTypeAnnotation (child argument) (child result)


alias : Environment -> Global -> Result MakeError (Maybe Alias)
alias environment ( package, home, name ) =
    Dict.get ( package, home ) environment.modules
        |> Result.fromMaybe (Errors.internal CompilerInvariant ("Missing type module " ++ home))
        |> Result.andThen
            (\owner ->
                owner.syntax.declarations
                    |> List.filterMap
                        (\declaration ->
                            case declaration of
                                AliasDeclaration value ->
                                    if value.name == name then
                                        Just value

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
                    |> List.head
                    |> Maybe.map
                        (\value ->
                            resolve environment owner value.typeAnnotation
                                |> Result.map (\body -> Just { args = value.generics, body = body })
                        )
                    |> Maybe.withDefault (Ok Nothing)
            )


expandAlias : Environment -> Global -> List Type -> Result MakeError (Maybe Type)
expandAlias environment global arguments =
    alias environment global
        |> Result.andThen
            (\definition ->
                case definition of
                    Nothing ->
                        Ok Nothing

                    Just value ->
                        if List.length value.args /= List.length arguments then
                            Err (Errors.unsupported Location.Project MainTypeSubstitution)

                        else
                            Ok (Just (substitute (Dict.fromList (List.map2 Tuple.pair value.args arguments)) value.body))
            )


substitute : Dict String Type -> Type -> Type
substitute variables annotation =
    let
        child =
            substitute variables
    in
    case annotation of
        GenericType name ->
            Dict.get name variables |> Maybe.withDefault annotation

        Typed global arguments ->
            Typed global (List.map child arguments)

        Tupled items ->
            Tupled (List.map child items)

        Record extension fields ->
            let
                own =
                    List.map (\( name, value ) -> ( name, child value )) fields
            in
            case extension |> Maybe.andThen (\name -> Dict.get name variables) of
                Just (Record base inherited) ->
                    Record base (inherited ++ own)

                _ ->
                    Record extension own

        FunctionTypeAnnotation argument result ->
            FunctionTypeAnnotation (child argument) (child result)

        Unit ->
            Unit
