module ElmMake.MainType exposing (Main(..), classify)

import Dict
import ElmMake.Canonicalize as Canonicalize
import ElmMake.Errors as Errors
import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import ElmMake.Types as Types
import ElmSyntax.Abstract.Declaration exposing (Declaration(..))
import ElmSyntax.Abstract.Expression exposing (Expression(..))
import ElmSyntax.Abstract.TypeAnnotation as Type exposing (TypeAnnotation(..))
import ElmSyntax.Concrete.Declaration as Concrete
import ElmSyntax.Concrete.Node as Node
import Set exposing (Set)


{-| Determine the entry-point representation, not general type correctness.
Unsupported or unresolved main types fail instead of guessing an init wrapper.
-}
type Main
    = StaticHtml
    | ProgramWithUnitFlags


classify : Environment -> Module -> Result MakeError Main
classify environment entry =
    inferDeclaration environment Set.empty entry "main" 0


inferDeclaration : Environment -> Set Global -> Module -> String -> Int -> Result MakeError Main
inferDeclaration environment seen module_ name supplied =
    let
        global =
            ( module_.package, module_.name, name )

        functions =
            module_.syntax.declarations
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            FunctionDeclaration function ->
                                if function.declaration.name == name then
                                    Just function

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
    in
    if Set.member global seen then
        Err (Errors.unsupported (Location.File module_.file) (RecursiveMainType name))

    else
        case functions of
            [ function ] ->
                case function.signature of
                    Just signature ->
                        consumeArguments supplied signature.typeAnnotation
                            |> Result.andThen (Types.resolve environment module_)
                            |> Result.andThen (mainType environment Set.empty module_)

                    Nothing ->
                        if supplied /= List.length function.declaration.arguments then
                            Err (Errors.unsupported (Location.File module_.file) (PartialMainType name))

                        else
                            inferExpression environment (Set.insert global seen) module_ function.declaration.expression

            _ ->
                Err (Errors.fileError module_ NoMain)


consumeArguments : Int -> TypeAnnotation -> Result MakeError TypeAnnotation
consumeArguments count annotation =
    if count == 0 then
        Ok annotation

    else
        case annotation of
            FunctionTypeAnnotation _ result ->
                consumeArguments (count - 1) result

            _ ->
                Err (Errors.unsupported Location.Project MainTypeSubstitution)


inferExpression : Environment -> Set Global -> Module -> Expression -> Result MakeError Main
inferExpression environment seen module_ expression =
    let
        referenced qualifier name count =
            Canonicalize.resolve { module_ = module_, locals = Dict.empty, environment = environment, mode = developmentMode, origin = emptyOrigin, cyclicValues = Set.empty } qualifier name
                |> Result.andThen
                    (\( package, home, declaration ) ->
                        case Dict.get ( package, home ) environment.modules of
                            Just target ->
                                inferDeclaration environment seen target declaration count

                            Nothing ->
                                Err (Errors.naming module_ (MissingModule home))
                    )
    in
    case expression of
        Application (Identifier qualifier name) arguments ->
            referenced qualifier name (List.length arguments)

        Identifier qualifier name ->
            referenced qualifier name 0

        IfBlock _ yes no ->
            Result.map2 Tuple.pair
                (inferExpression environment seen module_ yes)
                (inferExpression environment seen module_ no)
                |> Result.andThen
                    (\( first, second ) ->
                        if first == second then
                            Ok first

                        else
                            Err (Errors.unsupported (Location.File module_.file) NonHtmlMain)
                    )

        _ ->
            Err (Errors.unsupported (Location.File module_.file) HigherOrderMain)


mainType : Environment -> Set Global -> Module -> Types.Type -> Result MakeError Main
mainType environment seen module_ annotation =
    case annotation of
        Types.Typed (( package, home, name ) as key) arguments ->
            Dict.get ( package, home ) environment.modules
                |> Result.fromMaybe (Errors.internal CompilerInvariant ("Missing main type module " ++ home))
                |> Result.andThen
                    (\owner ->
                        if key == ( "elm/virtual-dom", "VirtualDom", "Node" ) then
                            Ok StaticHtml

                        else if key == ( "elm/core", "Platform", "Program" ) then
                            case arguments of
                                flags :: _ :: _ :: [] ->
                                    resolveUnitFlags environment Set.empty module_ flags |> Result.map (\_ -> ProgramWithUnitFlags)

                                _ ->
                                    Err (Errors.unsupported (Location.File module_.file) UnsupportedFlags)

                        else if Set.member key seen then
                            let
                                result =
                                    Err (Errors.fileError owner (CyclicTypeAlias name))
                            in
                            case owner.source of
                                Nothing ->
                                    result

                                Just source ->
                                    source.declarations
                                        |> List.filterMap
                                            (\node ->
                                                case Node.value node of
                                                    Concrete.AliasDeclaration alias ->
                                                        if Node.value alias.name == name then
                                                            Just alias.name

                                                        else
                                                            Nothing

                                                    _ ->
                                                        Nothing
                                            )
                                        |> List.head
                                        |> Maybe.map (\node -> Errors.at owner.file (Node.range node) result)
                                        |> Maybe.withDefault result

                        else
                            Types.expandAlias environment key arguments
                                |> Result.andThen
                                    (\expanded ->
                                        case expanded of
                                            Just target ->
                                                mainType environment (Set.insert key seen) owner target

                                            Nothing ->
                                                Err (Errors.unsupported (Location.File module_.file) (UnsupportedMainType (owner.name ++ "." ++ name)))
                                    )
                    )

        _ ->
            Err (Errors.unsupported (Location.File module_.file) NonHtmlMain)


resolveUnitFlags : Environment -> Set Global -> Module -> Types.Type -> Result MakeError ()
resolveUnitFlags environment seen module_ annotation =
    case annotation of
        Types.Unit ->
            Ok ()

        Types.Typed (( package, home, name ) as key) arguments ->
            Dict.get ( package, home ) environment.modules
                |> Result.fromMaybe (Errors.internal CompilerInvariant ("Missing flags type module " ++ home))
                |> Result.andThen
                    (\owner ->
                        if Set.member key seen then
                            Err (Errors.fileError owner (CyclicTypeAlias name))

                        else
                            Types.expandAlias environment key arguments
                                |> Result.andThen
                                    (\expanded ->
                                        expanded
                                            |> Maybe.map (resolveUnitFlags environment (Set.insert key seen) owner)
                                            |> Maybe.withDefault (Err (Errors.unsupported (Location.File module_.file) UnsupportedFlags))
                                    )
                    )

        _ ->
            Err (Errors.unsupported (Location.File module_.file) UnsupportedFlags)
