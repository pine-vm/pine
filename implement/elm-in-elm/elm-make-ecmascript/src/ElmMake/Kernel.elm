module ElmMake.Kernel exposing (compile, compileWithMode, find, shortName, tokenLength)

import Char
import Dict exposing (Dict)
import ElmMake.Canonicalize as Canonicalize
import ElmMake.Errors as Errors
import ElmMake.Model exposing (..)
import ElmMake.SourceLocation as Location
import ElmSyntax.Abstract.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Set


find : Environment -> String -> String -> Result MakeError ( String, String )
find environment package name =
    let
        candidates =
            Dict.keys environment.kernels
                |> List.filter (\( _, moduleName ) -> moduleName == name)
                |> List.map Tuple.first
    in
    if Dict.member ( package, name ) environment.kernels then
        Ok ( package, name )

    else
        case candidates of
            [ owner ] ->
                Ok ( owner, name )

            [] ->
                Err (Errors.internal MissingPackageSources ("Kernel source " ++ name ++ " required by " ++ package))

            _ ->
                Err (Errors.internal InvalidPackageSources ("Ambiguous kernel source " ++ name ++ " required by " ++ package))


type alias Scan =
    { chunksRev : List String
    , dependencies : Set.Set Global
    , enums : Dict ( Int, String ) Int
    , fields : Dict String Int
    }


compile : Environment -> String -> String -> String -> Result MakeError EmittedCode
compile =
    compileWithMode developmentMode


compileWithMode : Mode -> Environment -> String -> String -> String -> Result MakeError EmittedCode
compileWithMode mode environment package name source =
    case List.head (String.indexes "*/" source) of
        Nothing ->
            Err (Errors.internal InvalidPackageSources (name ++ ": Kernel JavaScript must start with an import comment."))

        Just headerEnd ->
            if not (String.startsWith "/*" source) then
                Err (Errors.internal InvalidPackageSources (name ++ ": Kernel JavaScript must start with an import comment."))

            else
                Canonicalize.parseModule ("module KernelImports exposing (..)\n" ++ String.slice 2 headerEnd source)
                    |> Result.mapError (\( error, _ ) -> Errors.internal InvalidPackageSources error.message)
                    |> Result.andThen
                        (\syntax ->
                            let
                                module_ =
                                    { package = package, name = "KernelImports", path = name, syntax = syntax, origins = Dict.empty, file = Location.ApplicationFile [ name ], source = Nothing }

                                table =
                                    syntax.imports
                                        |> traverse
                                            (\import_ ->
                                                let
                                                    imported =
                                                        String.join "." import_.moduleName
                                                in
                                                if String.startsWith "Elm.Kernel." imported then
                                                    find environment package imported
                                                        |> Result.map (\( owner, _ ) -> { import_ = import_, prefix = String.dropLeft 11 imported, owner = owner, moduleName = imported })

                                                else
                                                    Canonicalize.resolveModule environment module_ imported
                                                        |> Result.map (\found -> { import_ = import_, prefix = String.join "_" (Maybe.withDefault import_.moduleName import_.moduleAlias), owner = found.package, moduleName = imported })
                                            )
                                        |> Result.andThen
                                            (traverse
                                                (\binding ->
                                                    case binding.import_.exposingList of
                                                        Just (Explicit names) ->
                                                            names
                                                                |> traverse
                                                                    (\exposed ->
                                                                        case exposed of
                                                                            FunctionExpose value ->
                                                                                Ok ( binding.prefix ++ "_" ++ value, ( binding.owner, binding.moduleName, value ) )

                                                                            TypeOrAliasExpose value ->
                                                                                Ok ( binding.prefix ++ "_" ++ value, ( binding.owner, binding.moduleName, value ) )

                                                                            _ ->
                                                                                Err (Errors.internal InvalidPackageSources (name ++ ": Unsupported kernel import exposure."))
                                                                    )

                                                        _ ->
                                                            Err (Errors.internal InvalidPackageSources (name ++ ": Kernel imports must explicitly expose individual names."))
                                                )
                                            )
                                        |> Result.map (List.concat >> Dict.fromList)
                            in
                            table
                                |> Result.andThen
                                    (\imports ->
                                        scan mode
                                            imports
                                            (String.dropLeft (headerEnd + 2) source)
                                            { chunksRev = [], dependencies = Set.empty, enums = Dict.empty, fields = Dict.empty }
                                    )
                        )


scan : Mode -> Dict String Global -> String -> Scan -> Result MakeError EmittedCode
scan mode imports remaining state =
    case List.head (String.indexes "__" remaining) of
        Nothing ->
            Ok
                { text = String.concat (List.reverse (remaining :: state.chunksRev))
                , dependencies = state.dependencies
                }

        Just offset ->
            let
                after =
                    String.dropLeft (offset + 2) remaining

                token =
                    String.left (tokenLength 0 after) after

                finish replacement next =
                    scan mode
                        imports
                        (String.dropLeft (String.length token) after)
                        { next | chunksRev = replacement :: String.left offset remaining :: next.chunksRev }
            in
            case String.uncons token of
                Nothing ->
                    Err (Errors.internal InvalidPackageSources "Empty kernel substitution.")

                Just ( first, rest ) ->
                    if token == "DEBUG" then
                        finish
                            (if mode.flavor == Optimize then
                                "_UNUSED"

                             else
                                ""
                            )
                            state

                    else if token == "PROD" then
                        finish
                            (if mode.flavor == Optimize then
                                ""

                             else
                                "_UNUSED"
                            )
                            state

                    else if first == '$' then
                        finish (Dict.get rest mode.fields |> Maybe.withDefault rest) state

                    else if Char.isDigit first then
                        let
                            key =
                                ( Char.toCode first, token )

                            index =
                                Dict.get key state.enums
                                    |> Maybe.withDefault (Dict.keys state.enums |> List.filter (\( family, _ ) -> family == Char.toCode first) |> List.length)
                        in
                        finish (String.fromInt index) { state | enums = Dict.insert key index state.enums }

                    else if Char.isLower first then
                        let
                            index =
                                Dict.get token state.fields |> Maybe.withDefault (Dict.size state.fields)
                        in
                        finish (shortName index) { state | fields = Dict.insert token index state.fields }

                    else
                        case Dict.get token imports of
                            Nothing ->
                                Err (Errors.internal InvalidPackageSources ("Kernel reference is not imported: " ++ token))

                            Just global ->
                                let
                                    ( _, home, _ ) =
                                        global
                                in
                                finish (globalName global)
                                    (if home == "Elm.Kernel.Debugger" && mode.flavor /= Debug then
                                        state

                                     else
                                        { state | dependencies = Set.insert global state.dependencies }
                                    )


tokenLength : Int -> String -> Int
tokenLength offset source =
    case String.uncons (String.slice offset (offset + 1) source) of
        Just ( char, _ ) ->
            if Char.isAlphaNum char || char == '_' || char == '$' then
                tokenLength (offset + 1) source

            else
                offset

        Nothing ->
            offset


shortName : Int -> String
shortName index =
    let
        alphabet =
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$0123456789"

        digit n =
            String.slice n (n + 1) alphabet

        encode width n =
            if width == 1 then
                digit n

            else
                encode (width - 1) (n // 64) ++ digit (modBy 64 n)

        reserved =
            String.words "do if in NaN int for new try var let null true eval byte char goto long case else this void with enum false final float short break catch throw while class const super yield double native throws delete return switch typeof export import public static boolean default finally extends package private Infinity abstract volatile function continue debugger undefined arguments transient interface protected instanceof implements synchronized"

        longer width n =
            let
                blockSize =
                    54 * (64 ^ (width - 1))

                bad =
                    reserved |> List.filter (\keyword -> String.length keyword == width) |> List.sort |> List.reverse

                available =
                    blockSize - List.length bad

                name =
                    encode width n
            in
            if n >= available then
                longer (width + 1) (n - available)

            else
                case bad |> List.indexedMap Tuple.pair |> List.filter (\( _, keyword ) -> keyword == name) |> List.head of
                    Just ( offset, _ ) ->
                        encode width (blockSize - 1 - offset)

                    Nothing ->
                        name
    in
    if index < 53 then
        digit index

    else
        longer 2 (index - 53)
