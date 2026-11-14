module ElmMake.DebugMetadata exposing (encode)

import Dict
import ElmMake.Canonicalize as Canonicalize
import ElmMake.DependencyGraph as Graph
import ElmMake.Errors as Errors
import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import ElmMake.Types as Types exposing (Type(..))
import ElmSyntax.Abstract.Declaration exposing (Declaration(..))
import ElmSyntax.Abstract.Expression exposing (Expression(..), FunctionStruct, RecordSetter)
import ElmSyntax.Abstract.TypeAnnotation as Syntax
import Json.Encode as Json
import Set exposing (Set)


type alias Extracted =
    { text : String, aliases : Set Global, unions : Set Global }


empty : String -> Extracted
empty text =
    { text = text, aliases = Set.empty, unions = Set.empty }


merge : String -> List Extracted -> Extracted
merge text items =
    { text = text
    , aliases = List.foldl (Set.union << .aliases) Set.empty items
    , unions = List.foldl (Set.union << .unions) Set.empty items
    }


publicName : Global -> String
publicName ( _, home, name ) =
    home ++ "." ++ name


encode : Environment -> Module -> Result MakeError String
encode environment entry =
    mainMessage environment Set.empty entry "main"
        |> Result.andThen (extract environment 0 Set.empty)
        |> Result.andThen
            (\message ->
                transitive environment Set.empty Set.empty message.aliases message.unions
                    |> Result.map
                        (\( aliases, unions ) ->
                            Json.object
                                [ ( "versions", Json.object [ ( "elm", Json.string "0.19.1" ) ] )
                                , ( "types", Json.object [ ( "message", Json.string message.text ), ( "aliases", Json.object aliases ), ( "unions", Json.object unions ) ] )
                                ]
                                |> Json.encode 0
                        )
            )


findFunction : Module -> String -> Maybe FunctionStruct
findFunction module_ name =
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
        |> List.head


unresolved : Module -> MakeError
unresolved module_ =
    Errors.unsupported (Location.File module_.file) HigherOrderMain


programMessage : Environment -> Module -> Set Global -> Type -> Result MakeError Type
programMessage environment module_ seen annotation =
    case annotation of
        Typed (( _, _, name ) as key) arguments ->
            if key == ( "elm/core", "Platform", "Program" ) then
                case arguments of
                    [ _, _, message ] ->
                        Ok message

                    _ ->
                        Err (unresolved module_)

            else if Set.member key seen then
                Err (Errors.fileError module_ (CyclicTypeAlias name))

            else
                Types.expandAlias environment key arguments
                    |> Result.andThen
                        (\expanded ->
                            expanded
                                |> Maybe.map (programMessage environment module_ (Set.insert key seen))
                                |> Maybe.withDefault (Err (unresolved module_))
                        )

        _ ->
            Err (unresolved module_)


mainMessage : Environment -> Set Global -> Module -> String -> Result MakeError Type
mainMessage environment seen module_ name =
    let
        key =
            ( module_.package, module_.name, name )
    in
    if Set.member key seen then
        Err (unresolved module_)

    else
        findFunction module_ name
            |> Result.fromMaybe (unresolved module_)
            |> Result.andThen
                (\function ->
                    case function.signature of
                        Just signature ->
                            Types.resolve environment module_ signature.typeAnnotation
                                |> Result.andThen (programMessage environment module_ Set.empty)

                        Nothing ->
                            inferMessage environment (Set.insert key seen) module_ function.declaration.expression
                )


inferMessage : Environment -> Set Global -> Module -> Expression -> Result MakeError Type
inferMessage environment seen module_ expression =
    case expression of
        Application (Identifier qualifier constructor) [ RecordExpr fields ] ->
            Canonicalize.resolve (Canonicalize.interfaceScope environment module_) qualifier constructor
                |> Result.andThen
                    (\global ->
                        if
                            List.member global
                                [ ( "elm/browser", "Browser", "sandbox" )
                                , ( "elm/browser", "Browser", "element" )
                                , ( "elm/browser", "Browser", "document" )
                                , ( "elm/browser", "Browser", "application" )
                                , ( "elm/core", "Platform", "worker" )
                                ]
                        then
                            updateMessage environment module_ fields

                        else
                            Err (unresolved module_)
                    )

        Identifier qualifier name ->
            referencedFunction environment module_ qualifier name
                |> Result.andThen (\( owner, function ) -> mainMessage environment seen owner function.declaration.name)

        _ ->
            Err (unresolved module_)


referencedFunction : Environment -> Module -> List String -> String -> Result MakeError ( Module, FunctionStruct )
referencedFunction environment module_ qualifier name =
    Canonicalize.resolve (Canonicalize.interfaceScope environment module_) qualifier name
        |> Result.andThen
            (\( package, home, value ) ->
                Dict.get ( package, home ) environment.modules
                    |> Maybe.andThen (\owner -> findFunction owner value |> Maybe.map (Tuple.pair owner))
                    |> Result.fromMaybe (unresolved module_)
            )


updateMessage : Environment -> Module -> List RecordSetter -> Result MakeError Type
updateMessage environment module_ fields =
    fields
        |> List.filter (\field -> field.fieldName == "update")
        |> List.head
        |> Result.fromMaybe (unresolved module_)
        |> Result.andThen
            (\field ->
                case field.value of
                    Identifier qualifier name ->
                        referencedFunction environment module_ qualifier name
                            |> Result.andThen
                                (\( owner, function ) ->
                                    case Maybe.map .typeAnnotation function.signature of
                                        Just (Syntax.FunctionTypeAnnotation message _) ->
                                            Types.resolve environment owner message

                                        _ ->
                                            Err (unresolved module_)
                                )

                    _ ->
                        Err (unresolved module_)
            )


extract : Environment -> Int -> Set Global -> Type -> Result MakeError Extracted
extract environment precedence seen annotation =
    let
        children values =
            traverse (extract environment 2 seen) values

        grouped required text =
            if precedence > required then
                "(" ++ text ++ ")"

            else
                text
    in
    case annotation of
        Unit ->
            Ok (empty "()")

        GenericType name ->
            Ok (empty name)

        FunctionTypeAnnotation argument result ->
            Result.map2
                (\left right -> merge (grouped 0 (left.text ++ " -> " ++ right.text)) [ left, right ])
                (extract environment 1 seen argument)
                (extract environment 0 seen result)

        Tupled [ item ] ->
            extract environment precedence seen item

        Tupled items ->
            traverse (extract environment 0 seen) items |> Result.map (\parts -> merge ("( " ++ String.join ", " (List.map .text parts) ++ " )") parts)

        Record extension fields ->
            record environment seen extension fields

        Typed key arguments ->
            Types.alias environment key
                |> Result.andThen
                    (\alias ->
                        children arguments
                            |> Result.andThen
                                (\parts ->
                                    let
                                        rendered =
                                            publicName key
                                                ++ (if List.isEmpty parts then
                                                        ""

                                                    else
                                                        " " ++ String.join " " (List.map .text parts)
                                                   )

                                        base =
                                            merge
                                                (if List.isEmpty parts then
                                                    rendered

                                                 else
                                                    grouped 1 rendered
                                                )
                                                parts
                                    in
                                    case alias of
                                        Nothing ->
                                            Ok { base | unions = Set.insert key base.unions }

                                        Just value ->
                                            if Set.member key seen then
                                                Ok { base | aliases = Set.insert key base.aliases }

                                            else
                                                extract environment 0 (Set.insert key seen) value.body
                                                    |> Result.map
                                                        (\body ->
                                                            { base | aliases = Set.insert key (Set.union base.aliases body.aliases), unions = Set.union base.unions body.unions }
                                                        )
                                )
                    )


record : Environment -> Set Global -> Maybe String -> List ( String, Type ) -> Result MakeError Extracted
record environment seen extension fields =
    traverse (\( name, field ) -> extract environment 0 seen field |> Result.map (Tuple.pair name)) fields
        |> Result.map
            (\items ->
                merge
                    (if List.isEmpty items then
                        "{}"

                     else
                        "{ "
                            ++ (extension |> Maybe.map (\name -> name ++ " | ") |> Maybe.withDefault "")
                            ++ String.join ", " (List.map (\( name, value ) -> name ++ " : " ++ value.text) items)
                            ++ " }"
                    )
                    (List.map Tuple.second items)
            )


transitive : Environment -> Set Global -> Set Global -> Set Global -> Set Global -> Result MakeError ( List ( String, Json.Value ), List ( String, Json.Value ) )
transitive environment seenAliases seenUnions nextAliases nextUnions =
    let
        aliases =
            Set.diff nextAliases seenAliases |> Graph.order

        unions =
            Set.diff nextUnions seenUnions |> Graph.order

        owner ( package, home, _ ) =
            Dict.get ( package, home ) environment.modules
                |> Result.fromMaybe (Errors.internal CompilerInvariant ("Missing debugger type module " ++ home))

        alias key =
            Types.alias environment key
                |> Result.andThen
                    (\definition ->
                        definition
                            |> Result.fromMaybe (Errors.internal CompilerInvariant ("Missing debugger alias " ++ publicName key))
                            |> Result.andThen
                                (\value ->
                                    extract environment 0 Set.empty value.body
                                        |> Result.map
                                            (\body ->
                                                ( ( publicName key, Json.object [ ( "args", Json.list Json.string value.args ), ( "type", Json.string body.text ) ] ), body )
                                            )
                                )
                    )

        union key =
            if key == ( "elm/core", "List", "List" ) then
                Ok ( ( "List.List", Json.object [ ( "args", Json.list Json.string [ "a" ] ), ( "tags", Json.object [] ) ] ), empty "" )

            else
                owner key
                    |> Result.andThen
                        (\module_ ->
                            module_.syntax.declarations
                                |> List.filterMap
                                    (\declaration ->
                                        case declaration of
                                            ChoiceTypeDeclaration value ->
                                                if value.name == third key then
                                                    Just value

                                                else
                                                    Nothing

                                            _ ->
                                                Nothing
                                    )
                                |> List.head
                                |> Result.fromMaybe (Errors.internal CompilerInvariant ("Missing debugger union " ++ publicName key))
                                |> Result.andThen
                                    (\value ->
                                        traverse
                                            (\constructor ->
                                                traverse (\annotation -> Types.resolve environment module_ annotation |> Result.andThen (extract environment 0 Set.empty)) constructor.arguments
                                                    |> Result.map (\parts -> ( ( constructor.name, Json.list (Json.string << .text) parts ), merge "" parts ))
                                            )
                                            value.constructors
                                            |> Result.map
                                                (\constructors ->
                                                    ( ( publicName key, Json.object [ ( "args", Json.list Json.string value.generics ), ( "tags", Json.object (List.map Tuple.first constructors) ) ] )
                                                    , merge "" (List.map Tuple.second constructors)
                                                    )
                                                )
                                    )
                        )
    in
    if List.isEmpty aliases && List.isEmpty unions then
        Ok ( [], [] )

    else
        Result.map2 Tuple.pair (traverse alias aliases) (traverse union unions)
            |> Result.andThen
                (\( aliasEntries, unionEntries ) ->
                    let
                        deps =
                            merge "" (List.map Tuple.second (aliasEntries ++ unionEntries))
                    in
                    transitive environment (Set.union seenAliases nextAliases) (Set.union seenUnions nextUnions) deps.aliases deps.unions
                        |> Result.map
                            (\( laterAliases, laterUnions ) ->
                                ( List.map Tuple.first aliasEntries ++ laterAliases, List.map Tuple.first unionEntries ++ laterUnions )
                            )
                )


third : Global -> String
third ( _, _, name ) =
    name
