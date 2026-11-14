module ElmMake.Errors exposing (at, collect, combine, fileError, internal, location, naming, problem, unsupported)

import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import ElmSyntax.Concrete.Range exposing (Range)


problem : Location.Location -> ProblemKind -> MakeError
problem where_ kind =
    CompilerErrors { location = where_, kind = kind } []


fileError : Module -> ProblemKind -> MakeError
fileError module_ =
    problem (Location.File module_.file)


naming : Module -> NameProblem -> MakeError
naming module_ kind =
    fileError module_ (NamingProblem kind)


unsupported : Location.Location -> UnsupportedCapability -> MakeError
unsupported where_ feature =
    UnsupportedFeature { location = where_, feature = feature }


internal : InternalProblemKind -> String -> MakeError
internal kind context =
    InternalFailure { kind = kind, context = context }


location : Location.FileId -> Range -> Result MakeError Location.Location
location file range =
    Location.fromRange range
        |> Result.map (Location.Source file)
        |> Result.mapError (internal InvalidSourceLocation)


{-| Attach the producing syntax node's range, never recover positions from prose.
Existing source spans are more specific and remain unchanged.
-}
at : Location.FileId -> Range -> Result MakeError a -> Result MakeError a
at file range result =
    let
        locate where_ =
            case where_ of
                Location.Source _ _ ->
                    Ok where_

                _ ->
                    location file range

        relocate error =
            case error of
                CompilerErrors first rest ->
                    traverse
                        (\item -> locate item.location |> Result.map (\where_ -> { item | location = where_ }))
                        (first :: rest)
                        |> Result.andThen
                            (\items ->
                                case items of
                                    head :: tail ->
                                        Err (CompilerErrors head tail)

                                    [] ->
                                        Err (internal CompilerInvariant "Lost a nonempty diagnostic collection.")
                            )

                UnsupportedFeature item ->
                    locate item.location |> Result.andThen (\where_ -> Err (UnsupportedFeature { item | location = where_ }))

                InternalFailure _ ->
                    Err error
    in
    case result of
        Ok value ->
            Ok value

        Err error ->
            relocate error


combine : MakeError -> MakeError -> MakeError
combine left right =
    case ( left, right ) of
        ( InternalFailure _, _ ) ->
            left

        ( _, InternalFailure _ ) ->
            right

        ( UnsupportedFeature _, _ ) ->
            left

        ( _, UnsupportedFeature _ ) ->
            right

        ( CompilerErrors first rest, CompilerErrors other remaining ) ->
            CompilerErrors first (rest ++ other :: remaining)


{-| Accumulate only independent work. Dependent phases still use Result.andThen.
Infrastructure or capability failures cannot be disguised as input diagnostics.
-}
collect : (a -> Result MakeError b) -> List a -> Result MakeError (List b)
collect map values =
    List.foldr
        (\value rest ->
            case ( map value, rest ) of
                ( Ok item, Ok items ) ->
                    Ok (item :: items)

                ( Err left, Err right ) ->
                    Err (combine left right)

                ( Err error, _ ) ->
                    Err error

                ( _, Err error ) ->
                    Err error
        )
        (Ok [])
        values
