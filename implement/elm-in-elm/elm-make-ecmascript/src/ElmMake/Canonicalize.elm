module ElmMake.Canonicalize exposing (constructorInfo, definition, definitionScope, exported, interfaceScope, parseModule, parseSourceModule, resolve, resolveModule, resolveOperator, resolveType)

import Dict
import ElmMake.Effects as Effects
import ElmMake.Errors as Errors
import ElmMake.LiteralSources as LiteralSources
import ElmMake.Model exposing (..)
import ElmSyntax.Abstract.ConvertFromConcrete as Abstract
import ElmSyntax.Abstract.Declaration as Declaration exposing (Declaration(..))
import ElmSyntax.Abstract.Exposing as Exposing exposing (Exposing(..), TopLevelExpose(..))
import ElmSyntax.Abstract.Expression
import ElmSyntax.Abstract.File exposing (File)
import ElmSyntax.Abstract.Module as Module
import ElmSyntax.Abstract.Pattern
import ElmSyntax.Abstract.TypeAnnotation as Type
import ElmSyntax.Concrete.File as Concrete
import ElmSyntax.Concrete.Node as Node
import ElmSyntax.Concrete.Parser.FromString as Parser
import Set


{-| Parsing depends only on the source text; unrelated files and packages are not inputs.
-}
parseModule : String -> Result ( Parser.ParseError, List Parser.ParseError ) File
parseModule source =
    parseSourceModule source |> Result.map .syntax


parseSourceModule : String -> Result ( Parser.ParseError, List Parser.ParseError ) { syntax : File, origins : Dict.Dict String SourceOrigin, source : Concrete.File }
parseSourceModule source =
    Parser.parseFileStructured source
        |> Result.mapError (\error -> ( error, [] ))
        |> Result.andThen
            (\file ->
                case
                    List.map
                        (\node ->
                            let
                                error =
                                    (Node.value node).parseError
                            in
                            { message = error.message, region = { start = error.location, end = error.location } }
                        )
                        file.incompleteDeclarations
                of
                    first :: rest ->
                        Err ( first, rest )

                    [] ->
                        Ok { syntax = Abstract.fromFile file, origins = LiteralSources.fromFile file, source = file }
            )


definition : Module -> String -> Maybe Definition
definition module_ name =
    if Effects.isLeaf module_ name then
        Just EffectLeaf

    else
        module_.syntax.declarations
            |> List.filterMap
                (\declaration ->
                    case declaration of
                        FunctionDeclaration function ->
                            if function.declaration.name == name then
                                Just (Function function.declaration.arguments function.declaration.expression)

                            else
                                Nothing

                        ChoiceTypeDeclaration choice ->
                            choice.constructors
                                |> List.filter (\constructor -> constructor.name == name)
                                |> List.head
                                |> Maybe.map (\constructor -> Constructor (List.length constructor.arguments))

                        AliasDeclaration alias ->
                            case alias.typeAnnotation of
                                Type.Record fields ->
                                    if alias.name == name then
                                        Just
                                            (Function
                                                (List.map (\field -> ElmSyntax.Abstract.Pattern.VarPattern field.fieldName) fields)
                                                (ElmSyntax.Abstract.Expression.RecordExpr
                                                    (List.map (\field -> { fieldName = field.fieldName, value = ElmSyntax.Abstract.Expression.Identifier [] field.fieldName }) fields)
                                                )
                                            )

                                    else
                                        Nothing

                                _ ->
                                    Nothing

                        PortDeclaration _ ->
                            Nothing

                        InfixDeclaration _ ->
                            Nothing
                )
            |> List.head


constructorInfo : Environment -> Global -> Maybe { arity : Int, alternatives : Int, index : Int, enumeration : Bool }
constructorInfo environment ( package, home, name ) =
    Dict.get ( package, home ) environment.modules
        |> Maybe.andThen
            (\module_ ->
                module_.syntax.declarations
                    |> List.filterMap
                        (\declaration ->
                            case declaration of
                                ChoiceTypeDeclaration choice ->
                                    choice.constructors
                                        |> List.indexedMap Tuple.pair
                                        |> List.filter (\( _, constructor ) -> constructor.name == name)
                                        |> List.head
                                        |> Maybe.map
                                            (\( index, constructor ) ->
                                                { arity = List.length constructor.arguments
                                                , alternatives = List.length choice.constructors
                                                , index = index
                                                , enumeration = List.all (.arguments >> List.isEmpty) choice.constructors
                                                }
                                            )

                                _ ->
                                    Nothing
                        )
                    |> List.head
            )


moduleExposing : Module -> Exposing
moduleExposing module_ =
    case module_.syntax.moduleDefinition of
        Module.NormalModule data ->
            data.exposingList

        Module.PortModule data ->
            data.exposingList

        Module.EffectModule data ->
            data.exposingList


exported : Module -> String -> Bool
exported module_ name =
    exposedBy module_ (moduleExposing module_) name


exposedBy : Module -> Exposing -> String -> Bool
exposedBy module_ exposing_ name =
    case exposing_ of
        All ->
            definition module_ name /= Nothing || operatorFunction module_ name /= Nothing

        Explicit names ->
            List.any
                (\expose ->
                    case expose of
                        FunctionExpose value ->
                            value == name

                        InfixExpose value ->
                            value == name

                        TypeOrAliasExpose value ->
                            value == name

                        TypeExpose type_ ->
                            type_.name == name
                                || (type_.exposesConstructors
                                        && List.any
                                            (\declaration ->
                                                case declaration of
                                                    ChoiceTypeDeclaration choice ->
                                                        choice.name == type_.name && List.any (\constructor -> constructor.name == name) choice.constructors

                                                    _ ->
                                                        False
                                            )
                                            module_.syntax.declarations
                                   )
                )
                names


resolveModule : Environment -> Module -> String -> Result MakeError Module
resolveModule environment importer name =
    let
        local =
            Dict.get ( importer.package, name ) environment.modules

        candidates =
            Dict.get importer.package environment.dependencies
                |> Maybe.withDefault []
                |> List.filterMap (\package -> Dict.get ( package, name ) environment.modules)
                |> List.filter (\candidate -> Dict.get candidate.package environment.exposedModules |> Maybe.withDefault [] |> List.member name)
    in
    case local of
        Just found ->
            Ok found

        Nothing ->
            case List.filter (\candidate -> candidate.package /= importer.package) candidates of
                [ found ] ->
                    Ok found

                [] ->
                    Err (Errors.naming importer (MissingModule name))

                _ ->
                    Err (Errors.naming importer (AmbiguousModule name))


operatorFunction : Module -> String -> Maybe String
operatorFunction module_ operator =
    module_.syntax.declarations
        |> List.filterMap
            (\declaration ->
                case declaration of
                    InfixDeclaration infix ->
                        if infix.operator == operator then
                            Just infix.functionName

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head


resolveOperator : Scope -> String -> Result MakeError Global
resolveOperator scope operator =
    let
        target module_ =
            operatorFunction module_ operator
                |> Maybe.map (\name -> ( module_.package, module_.name, name ))
    in
    case target scope.module_ of
        Just global ->
            Ok global

        Nothing ->
            scope.module_.syntax.imports
                |> traverse
                    (\import_ ->
                        resolveModule scope.environment scope.module_ (String.join "." import_.moduleName)
                            |> Result.map
                                (\module_ ->
                                    if exported module_ operator && (import_.exposingList |> Maybe.map (\exposing_ -> exposedBy module_ exposing_ operator) |> Maybe.withDefault False) then
                                        target module_

                                    else
                                        Nothing
                                )
                    )
                |> Result.andThen
                    (\candidates ->
                        case List.filterMap identity candidates of
                            [ global ] ->
                                Ok global

                            [] ->
                                Err (Errors.naming scope.module_ (UnknownValue [] operator))

                            _ ->
                                Err (Errors.naming scope.module_ (AmbiguousName operator))
                    )


implicitModule : String -> Maybe String
implicitModule name =
    case name of
        "Cmd" ->
            Just "Platform.Cmd"

        "Sub" ->
            Just "Platform.Sub"

        _ ->
            if List.member name [ "Basics", "String", "Char", "List", "Maybe", "Result", "Tuple", "Debug", "Platform", "Platform.Cmd", "Platform.Sub" ] then
                Just name

            else
                Nothing


resolveType : Scope -> List String -> String -> Result MakeError Module
resolveType scope qualifier name =
    let
        importer =
            scope.module_

        primitive module_ =
            module_.package == "elm/core" && List.member ( module_.name, name ) [ ( "Basics", "Int" ), ( "Basics", "Float" ), ( "String", "String" ), ( "Char", "Char" ), ( "List", "List" ) ]

        exists module_ =
            primitive module_
                || List.any
                    (\declaration ->
                        case declaration of
                            ChoiceTypeDeclaration choice ->
                                choice.name == name

                            AliasDeclaration alias ->
                                alias.name == name

                            _ ->
                                False
                    )
                    module_.syntax.declarations

        exposed exposing_ =
            case exposing_ of
                All ->
                    True

                Explicit names ->
                    List.any
                        (\item ->
                            case item of
                                TypeOrAliasExpose value ->
                                    value == name

                                TypeExpose value ->
                                    value.name == name

                                _ ->
                                    False
                        )
                        names

        fromImport import_ =
            resolveModule scope.environment importer (String.join "." import_.moduleName)
                |> Result.andThen
                    (\target ->
                        if exists target && (primitive target || exposed (moduleExposing target)) then
                            Ok target

                        else
                            Err (Errors.naming importer (NotExposed target.name name))
                    )

        implicitHome =
            if name == "Program" then
                "Platform"

            else if name == "Cmd" then
                "Platform.Cmd"

            else if name == "Sub" then
                "Platform.Sub"

            else if List.member name [ "String", "Char", "List", "Maybe", "Result" ] then
                name

            else
                "Basics"

        implicit =
            resolveModule scope.environment importer implicitHome
                |> Result.andThen
                    (\target ->
                        if exists target && (primitive target || exposed (moduleExposing target)) then
                            Ok target

                        else
                            Err (Errors.naming importer (UnknownType [] name))
                    )
    in
    if List.isEmpty qualifier && exists importer then
        Ok importer

    else if List.isEmpty qualifier then
        case List.filter
                (\import_ ->
                    (import_.exposingList |> Maybe.map exposed |> Maybe.withDefault False)
                        && (resolveModule scope.environment importer (String.join "." import_.moduleName)
                                |> Result.map exists
                                |> Result.withDefault False
                           )
                )
                importer.syntax.imports of
            [ import_ ] ->
                fromImport import_

            [] ->
                implicit

            _ ->
                Err (Errors.naming importer (AmbiguousName name))

    else
        case List.filter (\import_ -> Maybe.withDefault import_.moduleName import_.moduleAlias == qualifier) importer.syntax.imports of
            [ import_ ] ->
                fromImport import_

            [] ->
                case implicitModule (String.join "." qualifier) of
                    Just moduleName ->
                        resolveModule scope.environment importer moduleName
                            |> Result.andThen
                                (\target ->
                                    if exists target && (primitive target || exposed (moduleExposing target)) then
                                        Ok target

                                    else
                                        Err (Errors.naming importer (UnknownType qualifier name))
                                )

                    Nothing ->
                        Err (Errors.naming importer (UnknownType qualifier name))

            imports ->
                imports
                    |> traverse (\import_ -> resolveModule scope.environment importer (String.join "." import_.moduleName))
                    |> Result.andThen
                        (\modules ->
                            case List.filter (\target -> exists target && (primitive target || exposed (moduleExposing target))) modules of
                                [ target ] ->
                                    Ok target

                                [] ->
                                    Err (Errors.naming importer (UnknownType qualifier name))

                                _ ->
                                    Err (Errors.naming importer (AmbiguousModule (String.join "." qualifier)))
                        )


resolve : Scope -> List String -> String -> Result MakeError Global
resolve scope qualifier name =
    let
        importer =
            scope.module_

        own =
            ( importer.package, importer.name, name )

        explicitImports =
            importer.syntax.imports

        importedName import_ =
            String.join "." (Maybe.withDefault import_.moduleName import_.moduleAlias)

        fromImport import_ =
            resolveModule scope.environment importer (String.join "." import_.moduleName)
                |> Result.andThen
                    (\found ->
                        if exported found name then
                            Ok ( found.package, found.name, name )

                        else
                            Err (Errors.naming importer (NotExposed found.name name))
                    )

        implicit =
            if List.member name [ "Just", "Nothing" ] then
                Just "Maybe"

            else if List.member name [ "Ok", "Err" ] then
                Just "Result"

            else if name == "::" then
                Just "List"

            else
                Just "Basics"
    in
    if List.isEmpty qualifier then
        if definition importer name /= Nothing then
            Ok own

        else
            let
                matching =
                    explicitImports
                        |> List.filter
                            (\import_ ->
                                case ( import_.exposingList, resolveModule scope.environment importer (String.join "." import_.moduleName) ) of
                                    ( Just exposing_, Ok found ) ->
                                        exposedBy found exposing_ name && exported found name

                                    _ ->
                                        False
                            )
            in
            case matching of
                [ import_ ] ->
                    fromImport import_

                [] ->
                    case implicit of
                        Just moduleName ->
                            resolveModule scope.environment importer moduleName
                                |> Result.andThen
                                    (\found ->
                                        if exported found name then
                                            Ok ( found.package, found.name, name )

                                        else
                                            Err (Errors.naming importer (UnknownValue [] name))
                                    )

                        Nothing ->
                            Err (Errors.naming importer (UnknownValue [] name))

                _ ->
                    Err (Errors.naming importer (AmbiguousName name))

    else
        let
            moduleName =
                String.join "." qualifier
        in
        if String.startsWith "Elm.Kernel." moduleName then
            if String.startsWith "elm/" importer.package || String.startsWith "elm-explorations/" importer.package then
                Ok ( importer.package, moduleName, name )

            else
                Err (Errors.naming importer RestrictedKernelImport)

        else if moduleName == importer.name then
            if definition importer name /= Nothing then
                Ok own

            else
                Err (Errors.naming importer (UnknownValue qualifier name))

        else
            case List.filter (\import_ -> importedName import_ == moduleName) explicitImports of
                [ import_ ] ->
                    fromImport import_

                [] ->
                    case implicitModule moduleName of
                        Just home ->
                            resolveModule scope.environment importer home
                                |> Result.andThen
                                    (\found ->
                                        if exported found name then
                                            Ok ( found.package, found.name, name )

                                        else
                                            Err (Errors.naming importer (UnknownValue qualifier name))
                                    )

                        Nothing ->
                            Err (Errors.naming importer (UnknownValue qualifier name))

                imports ->
                    imports
                        |> traverse (\import_ -> resolveModule scope.environment importer (String.join "." import_.moduleName))
                        |> Result.andThen
                            (\modules ->
                                case List.filter (\target -> exported target name) modules of
                                    [ target ] ->
                                        Ok ( target.package, target.name, name )

                                    [] ->
                                        Err (Errors.naming importer (UnknownValue qualifier name))

                                    _ ->
                                        Err (Errors.naming importer (AmbiguousModule moduleName))
                            )


{-| Emission sees names, signatures, and constructor shapes, not unrelated source
bodies or package file trees. This keeps individual declaration calls cacheable.
-}
interfaceScope : Environment -> Module -> Scope
interfaceScope environment module_ =
    let
        interface moduleSyntax =
            let
                syntax =
                    moduleSyntax.syntax
            in
            { moduleSyntax
                | origins = Dict.empty
                , source = Nothing
                , syntax =
                    { syntax
                        | declarations =
                            List.map
                                (\declaration ->
                                    case declaration of
                                        FunctionDeclaration function ->
                                            let
                                                implementation =
                                                    function.declaration
                                            in
                                            FunctionDeclaration { function | declaration = { implementation | expression = ElmSyntax.Abstract.Expression.UnitExpr } }

                                        _ ->
                                            declaration
                                )
                                syntax.declarations
                    }
            }

        relevantPackages =
            module_.package :: Maybe.withDefault [] (Dict.get module_.package environment.dependencies)
    in
    { module_ = interface module_
    , locals = Dict.empty
    , mode = developmentMode
    , origin = emptyOrigin
    , cyclicValues = Set.empty
    , environment =
        { modules = Dict.filter (\( package, _ ) _ -> List.member package relevantPackages) environment.modules |> Dict.map (\_ moduleSyntax -> interface moduleSyntax)
        , kernels = Dict.empty
        , dependencies = Dict.singleton module_.package (Maybe.withDefault [] (Dict.get module_.package environment.dependencies))
        , exposedModules = Dict.filter (\package _ -> List.member package relevantPackages) environment.exposedModules
        }
    }


definitionScope : Environment -> Module -> String -> Scope
definitionScope environment module_ name =
    let
        scope =
            interfaceScope environment module_
    in
    { scope | origin = Dict.get name module_.origins |> Maybe.withDefault emptyOrigin }
