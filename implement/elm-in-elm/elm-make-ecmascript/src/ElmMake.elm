module ElmMake exposing (make)

import Dict exposing (Dict)
import ElmMake.Canonicalize as Canonicalize
import ElmMake.CyclicDeclarations as CyclicDeclarations
import ElmMake.DebugMetadata as DebugMetadata
import ElmMake.DependencyGraph as Graph
import ElmMake.Effects as Effects
import ElmMake.EmissionMode as EmissionMode
import ElmMake.EntryPoint as EntryPoint
import ElmMake.Errors as Errors
import ElmMake.JavaScript as JavaScript
import ElmMake.Kernel as Kernel
import ElmMake.LocalDeclarations as LocalDeclarations
import ElmMake.MainType as MainType
import ElmMake.Model exposing (..)
import ElmMake.Patterns as Patterns
import ElmMake.Runtime as Runtime
import ElmMake.SourceLocation as Location
import ElmMake.Validate as Validate
import ElmSyntax.Abstract.Declaration exposing (Declaration(..))
import ElmSyntax.Abstract.Expression exposing (Expression(..))
import ElmSyntax.Abstract.Module as ModuleSyntax
import ElmSyntax.Concrete.Node as Node
import Json.Decode as Decode
import Set exposing (Set)


type alias Manifest =
    { sourceDirectories : List String
    , dependencies : List String
    , exposedModules : List String
    }


make : Request -> Result MakeError String
make request =
    prepare request
        |> Result.andThen
            (\( environment, entry ) ->
                Validate.modules environment entry
                    |> Result.andThen
                        (\modules ->
                            validateFlavor request.flavor environment modules
                                |> Result.andThen (\_ -> MainType.classify environment entry)
                                |> Result.map (Tuple.pair modules)
                        )
                    |> Result.andThen
                        (\( modules, main ) ->
                            let
                                root =
                                    ( entry.package, entry.name, "main" )

                                mode =
                                    EmissionMode.prepare request.flavor environment modules

                                metadata =
                                    if request.flavor == Debug && main == MainType.ProgramWithUnitFlags then
                                        DebugMetadata.encode environment entry

                                    else
                                        Ok "0"
                            in
                            metadata
                                |> Result.andThen
                                    (\debugInfo ->
                                        let
                                            initialization =
                                                EntryPoint.compile mode debugInfo main root
                                        in
                                        traverse (normalizeGlobal environment) initialization.dependencies
                                            |> Result.andThen
                                                (\dependencies ->
                                                    visit mode environment root Dict.empty
                                                        |> Result.andThen (\nodes -> foldResult (visit mode environment) nodes dependencies)
                                                        |> Result.andThen
                                                            (\nodes ->
                                                                Dict.update root
                                                                    (Maybe.map (\node -> { node | dependencies = Graph.order (Set.fromList (dependencies ++ node.dependencies)) }))
                                                                    nodes
                                                                    |> CyclicDeclarations.allocate mode environment
                                                                    |> Result.andThen (\graph -> Graph.emit graph root)
                                                            )
                                                        |> Result.map (Tuple.pair initialization.initializer)
                                                )
                                    )
                        )
                    |> Result.map
                        (\( init, ( kernels, declarations ) ) ->
                            let
                                exports =
                                    List.foldr (\segment value -> "{" ++ JavaScript.quote segment ++ ":" ++ value ++ "}") ("{'init':" ++ init ++ "}") (String.split "." entry.name)

                                javascript =
                                    Runtime.script request.flavor kernels declarations exports
                            in
                            case request.outputType of
                                JavaScript ->
                                    javascript

                                Html ->
                                    Runtime.html entry.name javascript
                        )
            )


validateFlavor : Flavor -> Environment -> List Module -> Result MakeError ()
validateFlavor flavor environment modules =
    let
        inspect module_ =
            module_.syntax.declarations
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            FunctionDeclaration function ->
                                let
                                    bound =
                                        List.concatMap Patterns.names function.declaration.arguments |> List.map (Tuple.pair []) |> Set.fromList
                                in
                                Just (Set.diff (LocalDeclarations.references function.declaration.expression) bound)

                            _ ->
                                Nothing
                    )
                |> List.foldl Set.union Set.empty
                |> Set.toList
                |> traverse (\( qualifier, name ) -> Canonicalize.resolve (Canonicalize.interfaceScope environment module_) qualifier name)
                |> Result.map
                    (\globals ->
                        if List.any (\( package, home, _ ) -> package == "elm/core" && home == "Debug") globals then
                            Just module_.name

                        else
                            Nothing
                    )
    in
    if flavor == Optimize then
        modules
            |> List.filter (\module_ -> module_.package == "author/project")
            |> traverse inspect
            |> Result.andThen
                (\found ->
                    case List.filterMap identity found |> List.sort of
                        [] ->
                            Ok ()

                        names ->
                            Err (Errors.problem Location.Project (DebugRemnants names))
                )

    else
        Ok ()


prepare : Request -> Result MakeError ( Environment, Module )
prepare request =
    lookupFile request.manifestPath request.files
        |> Result.mapError
            (\message ->
                if List.any (\file -> file.path == request.manifestPath) request.files then
                    Errors.internal InvalidRequest message

                else
                    Errors.problem Location.Project NoManifest
            )
        |> Result.andThen
            (\text ->
                decodeApplicationManifest text
                    |> Result.mapError (Errors.problem (Location.File (Location.ApplicationFile request.manifestPath)) << ManifestProblem)
            )
        |> Result.andThen
            (\manifest ->
                let
                    packageNames =
                        List.map .name request.packages
                in
                if not (List.any (\file -> file.path == request.entryPoint) request.files) then
                    Err (Errors.problem Location.Project (EntryPointNotFound request.entryPoint))

                else
                    case List.filter (\name -> not (List.member name packageNames)) manifest.dependencies |> List.head of
                        Just missing ->
                            Err (Errors.internal MissingPackageSources missing)

                        Nothing ->
                            if List.length packageNames /= Set.size (Set.fromList packageNames) then
                                Err (Errors.internal InvalidRequest "Duplicate package entries supplied to make.")

                            else
                                Errors.collect (sourceDirectory request.manifestPath (List.take (List.length request.manifestPath - 1) request.manifestPath)) manifest.sourceDirectories
                                    |> Result.andThen
                                        (\directories ->
                                            let
                                                manifestDirectory =
                                                    List.take (List.length request.manifestPath - 1) request.manifestPath

                                                nestedProjects =
                                                    request.files
                                                        |> List.filter
                                                            (\file ->
                                                                List.reverse file.path |> List.head |> (==) (Just "elm.json")
                                                            )
                                                        |> List.map (\file -> List.take (List.length file.path - 1) file.path)
                                                        |> List.filter (\path -> List.length path > List.length manifestDirectory && isPrefix manifestDirectory path)

                                                appFiles =
                                                    request.files
                                                        |> List.filter (\file -> List.any (\directory -> isPrefix directory file.path) directories)
                                                        |> List.filter (\file -> not (List.any (\nested -> isPrefix nested file.path) nestedProjects))

                                                empty =
                                                    { modules = Dict.empty
                                                    , kernels = Dict.empty
                                                    , dependencies = Dict.singleton "author/project" manifest.dependencies
                                                    , exposedModules = Dict.empty
                                                    }
                                            in
                                            foldResult addPackage empty request.packages
                                                |> Result.andThen
                                                    (\env ->
                                                        Errors.collect (parseModuleFile "author/project" << (\file -> ( Location.ApplicationFile file.path, file ))) (List.filter isElmFile appFiles)
                                                            |> Result.andThen (foldResult addParsedModule env)
                                                    )
                                                |> Result.andThen
                                                    (\environment ->
                                                        case Dict.values environment.modules |> List.filter (\module_ -> module_.package == "author/project" && module_.path == String.join "/" request.entryPoint) of
                                                            [ entry ] ->
                                                                Ok ( environment, entry )

                                                            _ ->
                                                                Err (Errors.problem Location.Project (EntryPointNotFound request.entryPoint))
                                                    )
                                        )
            )


decodeApplicationManifest : String -> Result String Manifest
decodeApplicationManifest text =
    Decode.decodeString
        (Decode.map4
            (\type_ version directories dependencies -> ( type_, version, { sourceDirectories = directories, dependencies = dependencies, exposedModules = [] } ))
            (Decode.field "type" Decode.string)
            (Decode.field "elm-version" Decode.string)
            (Decode.field "source-directories" (Decode.list Decode.string))
            (Decode.at [ "dependencies", "direct" ] (Decode.keyValuePairs Decode.string) |> Decode.map (List.map Tuple.first))
        )
        text
        |> Result.mapError Decode.errorToString
        |> Result.andThen
            (\( type_, version, manifest ) ->
                if type_ /= "application" || version /= "0.19.1" || List.isEmpty manifest.sourceDirectories then
                    Err "Expected an Elm 0.19.1 application with nonempty source-directories."

                else
                    Ok manifest
            )


addPackage : Package -> Environment -> Result MakeError Environment
addPackage package environment =
    lookupFile [ "elm.json" ] package.files
        |> Result.mapError (\error -> Errors.internal MissingPackageSources (package.name ++ ": " ++ error))
        |> Result.andThen
            (\text ->
                Decode.decodeString
                    (Decode.map4 (\name version dependencies exposedModules -> ( ( name, version, dependencies ), exposedModules ))
                        (Decode.field "name" Decode.string)
                        (Decode.field "version" Decode.string)
                        (Decode.field "dependencies" (Decode.keyValuePairs Decode.string) |> Decode.map (List.map Tuple.first))
                        (Decode.field "exposed-modules"
                            (Decode.oneOf
                                [ Decode.list Decode.string
                                , Decode.keyValuePairs (Decode.list Decode.string) |> Decode.map (List.concatMap Tuple.second)
                                ]
                            )
                        )
                    )
                    text
                    |> Result.mapError (Decode.errorToString >> Errors.internal InvalidPackageSources)
            )
        |> Result.andThen
            (\( ( name, version, dependencies ), exposedModules ) ->
                if name /= package.name || version /= package.version then
                    Err (Errors.internal InvalidPackageSources ("Package identity differs from its manifest: " ++ package.name))

                else
                    let
                        sources =
                            List.filter (\file -> isPrefix [ "src" ] file.path) package.files

                        withKernels =
                            { environment
                                | dependencies = Dict.insert name dependencies environment.dependencies
                                , exposedModules = Dict.insert name exposedModules environment.exposedModules
                                , kernels =
                                    List.foldl
                                        (\file kernels ->
                                            if isKernelFile file then
                                                Dict.insert ( name, file.path |> List.drop 1 |> String.join "." |> String.dropRight 3 ) file.content kernels

                                            else
                                                kernels
                                        )
                                        environment.kernels
                                        sources
                            }
                    in
                    Errors.collect
                        (\file -> parseModuleFile name ( Location.PackageFile { name = package.name, version = package.version, path = file.path }, file ))
                        (List.filter isElmFile sources)
                        |> Result.andThen (foldResult addParsedModule withKernels)
            )


parseModuleFile : String -> ( Location.FileId, SourceFile ) -> Result MakeError Module
parseModuleFile package ( fileId, file ) =
    Canonicalize.parseSourceModule file.content
        |> Result.mapError
            (\( first, rest ) ->
                let
                    convert error =
                        case Errors.location fileId error.region of
                            Ok where_ ->
                                Errors.problem where_ (SyntaxProblem error.message)

                            Err invalid ->
                                invalid
                in
                List.foldl (\error previous -> Errors.combine previous (convert error)) (convert first) rest
            )
        |> Result.map
            (\parsed ->
                { package = package
                , name = ModuleSyntax.moduleName parsed.syntax.moduleDefinition |> String.join "."
                , path = String.join "/" file.path
                , syntax = parsed.syntax
                , origins = parsed.origins
                , file = fileId
                , source = Just parsed.source
                }
            )


addParsedModule : Module -> Environment -> Result MakeError Environment
addParsedModule module_ environment =
    let
        key =
            ( module_.package, module_.name )
    in
    if Dict.member key environment.modules then
        let
            result =
                Err (Errors.naming module_ (DuplicateModule module_.name))
        in
        case module_.source of
            Just source ->
                Errors.at module_.file (Node.range source.moduleDefinition) result

            Nothing ->
                result

    else
        Ok { environment | modules = Dict.insert key module_ environment.modules }


lookupFile : List String -> List SourceFile -> Result String String
lookupFile path files =
    case List.filter (\file -> file.path == path) files of
        [ file ] ->
            Ok file.content

        [] ->
            Err ("Missing file " ++ String.join "/" path)

        _ ->
            Err ("Duplicate file " ++ String.join "/" path)


sourceDirectory : List String -> List String -> String -> Result MakeError (List String)
sourceDirectory manifestPath base relative =
    let
        invalid details =
            Errors.problem (Location.File (Location.ApplicationFile manifestPath)) (ManifestProblem details)
    in
    if String.isEmpty relative || String.startsWith "/" relative then
        Err (invalid ("Expected a nonempty relative source directory: " ++ relative))

    else
        foldResult
            (\segment path ->
                if segment == "." || segment == "" then
                    Ok path

                else if segment == ".." then
                    if List.isEmpty path then
                        Err (invalid ("Source directory escapes the input tree: " ++ relative))

                    else
                        Ok (List.take (List.length path - 1) path)

                else if String.contains ":" segment || String.contains "\\" segment then
                    Err (invalid ("Invalid source directory: " ++ relative))

                else
                    Ok (path ++ [ segment ])
            )
            base
            (String.split "/" relative)


isPrefix : List a -> List a -> Bool
isPrefix prefix items =
    List.take (List.length prefix) items == prefix


isElmFile : SourceFile -> Bool
isElmFile file =
    String.endsWith ".elm" (String.join "/" file.path)
        && not (List.member "elm-stuff" file.path)


isKernelFile : SourceFile -> Bool
isKernelFile file =
    let
        filename =
            List.reverse file.path |> List.head |> Maybe.withDefault ""
    in
    isPrefix [ "src", "Elm", "Kernel" ] file.path
        && (case String.split "." filename of
                [ _, "js" ] ->
                    True

                _ ->
                    False
           )


normalizeGlobal : Environment -> Global -> Result MakeError Global
normalizeGlobal environment (( package, home, _ ) as global) =
    if String.startsWith "Elm.Kernel." home then
        Kernel.find environment package home |> Result.map (\( owner, _ ) -> ( owner, home, "$" ))

    else
        let
            ( _, _, name ) =
                global
        in
        if Dict.get ( package, home ) environment.modules |> Maybe.map (\module_ -> Effects.isLeaf module_ name) |> Maybe.withDefault False then
            Ok ( package, home, Effects.managerName )

        else
            Ok global


visit : Mode -> Environment -> Global -> Dict Global Graph.Node -> Result MakeError (Dict Global Graph.Node)
visit mode environment requested nodes =
    normalizeGlobal environment requested
        |> Result.andThen
            (\(( package, home, name ) as global) ->
                if Dict.member global nodes then
                    Ok nodes

                else
                    let
                        isKernel =
                            String.startsWith "Elm.Kernel." home

                        ordinary code =
                            { text = code.text, dependencies = Graph.order code.dependencies }

                        compiled =
                            if isKernel then
                                case Dict.get ( package, home ) environment.kernels of
                                    Nothing ->
                                        Err (Errors.internal MissingPackageSources ("Kernel source " ++ home))

                                    Just source ->
                                        Kernel.compileWithMode mode environment package home source |> Result.map ordinary

                            else
                                case Dict.get ( package, home ) environment.modules of
                                    Nothing ->
                                        Err (Errors.internal CompilerInvariant ("Missing module " ++ home ++ " for " ++ name))

                                    Just module_ ->
                                        if name == Effects.managerName then
                                            Effects.compile module_

                                        else
                                            Validate.moduleFeatures module_
                                                |> Result.andThen
                                                    (\_ ->
                                                        case Canonicalize.definition module_ name of
                                                            Nothing ->
                                                                Err (Errors.internal CompilerInvariant ("Cannot find declaration " ++ module_.name ++ "." ++ name))

                                                            Just definition_ ->
                                                                let
                                                                    scope =
                                                                        Canonicalize.definitionScope environment module_ name
                                                                in
                                                                JavaScript.compileDefinition { scope | mode = mode } name definition_ |> Result.map ordinary
                                                    )
                    in
                    compiled
                        |> Result.andThen
                            (\code ->
                                traverse (normalizeGlobal environment) code.dependencies
                                    |> Result.andThen
                                        (\dependencies ->
                                            let
                                                node =
                                                    { text = code.text, dependencies = dependencies, isKernel = isKernel, eager = eager }

                                                eager =
                                                    Dict.get ( package, home ) environment.modules
                                                        |> Maybe.andThen (\module_ -> Canonicalize.definition module_ name)
                                                        |> Maybe.map
                                                            (\definition ->
                                                                case definition of
                                                                    Function arguments _ ->
                                                                        List.isEmpty arguments

                                                                    Constructor arity ->
                                                                        arity == 0

                                                                    EffectLeaf ->
                                                                        False
                                                            )
                                                        |> Maybe.withDefault False
                                            in
                                            foldResult (visit mode environment) (Dict.insert global node nodes) node.dependencies
                                        )
                            )
            )
