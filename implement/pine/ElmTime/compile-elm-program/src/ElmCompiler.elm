module ElmCompiler exposing
    ( CompilationStack
    , ElmModuleInCompilation
    , ElmModuleTypeDeclaration(..)
    , ProjectParsedElmFile
    , applicableDeclarationFromConstructorExpression
    , compilationAndEmitStackFromModulesInCompilation
    , compileElmSyntaxExpression
    , compileElmSyntaxFunction
    , elmRecordTypeTagName
    , elmRecordTypeTagNameAsValue
    , elmStringTypeTagName
    , emitTypeDeclarationValue
    , expandElmInteractiveEnvironmentWithModules
    , expressionForDeconstructions
    , getDeclarationsFromEnvironment
    , parseModuleValue
    , parseTypeDeclarationFromValueTagged
    , pineFunctionForRecordAccessAsValue
    , pineFunctionForRecordUpdateAsValue
    , separateEnvironmentDeclarations
    , stringStartsWithUpper
    , valueFromString
    )

import Common
import Dict
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import FirCompiler
    exposing
        ( Deconstruction(..)
        , EmitStack
        , Expression(..)
        , countListElementsExpression
        , emitWrapperForPartialApplication
        , equalCondition
        , equalCondition_Pine
        , estimatePineValueSize
        , evaluateAsIndependentExpression
        , listItemFromIndexExpression
        , listItemFromIndexExpression_Pine
        , listSkipExpression
        , listSkipExpression_Pine
        , pineKernel_ListHead
        , pineKernel_ListHead_Pine
        )
import List.Extra
import Pine
import Set


type alias ProjectParsedElmFile =
    { fileText : String
    , parsedModule : Elm.Syntax.File.File
    }


type alias CompilationStack =
    { moduleAliases : Dict.Dict (List String) (List String)
    , availableModules : Dict.Dict (List String) ElmModuleInCompilation
    , inlineableDeclarations : Dict.Dict String (List Expression -> Expression)
    , elmValuesToExposeToGlobal : Dict.Dict String (List String)
    , localTypeDeclarations : Dict.Dict String ElmModuleTypeDeclaration
    , depth : Int
    }


type alias ElmFunctionDeclarationStruct =
    { arguments : List Elm.Syntax.Pattern.Pattern
    , expression : Elm.Syntax.Expression.Expression
    }


type alias ModuleImportStatement =
    { canonicalModuleName : List String
    , localModuleName : List String
    , exposingList : Maybe ModuleImportExposing
    }


type ModuleImportExposing
    = ExposingAll
    | ExposingSelected (List ModuleImportTopLevelExpose)


type alias ModuleImportTopLevelExpose =
    { name : String
    , open : Bool
    }


type alias ModuleImports =
    { importedModules : Dict.Dict (List String) ElmModuleInCompilation
    , importedFunctions : Dict.Dict String Pine.Value
    , importedTypes : Dict.Dict String ElmModuleTypeDeclaration
    }


type alias ElmModuleInCompilation =
    { functionDeclarations : Dict.Dict String Pine.Value
    , typeDeclarations : Dict.Dict String ElmModuleTypeDeclaration
    }


type alias ModuleImportsMerged =
    { modulesDeclarationsFlat : Dict.Dict String Pine.Value
    , choiceTypeTagConstructorDeclarations : Dict.Dict String { argumentsCount : Int }
    , recordConstructorsFieldsNames : Dict.Dict String (List String)
    }


type ElmModuleTypeDeclaration
    = ElmModuleChoiceTypeDeclaration ElmModuleChoiceType
    | ElmModuleRecordTypeDeclaration (List String)


type alias ElmModuleChoiceType =
    { tags : Dict.Dict String { argumentsCount : Int }
    }


elmStringTypeTagName : String
elmStringTypeTagName =
    "String"


elmStringTypeTagNameAsValue : Pine.Value
elmStringTypeTagNameAsValue =
    Pine.valueFromString elmStringTypeTagName


elmRecordTypeTagName : String
elmRecordTypeTagName =
    "Elm_Record"


elmRecordTypeTagNameAsValue : Pine.Value
elmRecordTypeTagNameAsValue =
    Pine.valueFromString elmRecordTypeTagName


operatorPrecendencePriority : Dict.Dict String Int
operatorPrecendencePriority =
    [ ( "<|", 0 )
    , ( "|>", 0 )
    , ( "||", 2 )
    , ( "&&", 3 )
    , ( "==", 4 )
    , ( "/=", 4 )
    , ( "<", 4 )
    , ( ">", 4 )
    , ( "<=", 4 )
    , ( ">=", 4 )
    , ( "++", 5 )
    , ( "+", 6 )
    , ( "-", 6 )
    , ( "*", 7 )
    , ( "//", 7 )
    , ( "/", 7 )
    , ( "^", 8 )
    , ( "<<", 9 )
    , ( ">>", 9 )
    ]
        |> Dict.fromList


autoImportedModulesNames : List (List String)
autoImportedModulesNames =
    autoImportedModulesExposingTagsNames
        ++ [ [ "Char" ]
           , [ "Tuple" ]
           ]


autoImportedModulesExposingTagsNames : List (List String)
autoImportedModulesExposingTagsNames =
    [ [ "Basics" ]
    , [ "Maybe" ]
    , [ "List" ]
    , [ "String" ]
    , [ "Result" ]
    ]


elmValuesToExposeToGlobalDefault : Dict.Dict String (List String)
elmValuesToExposeToGlobalDefault =
    [ ( "LT", [ "Basics" ] )
    , ( "EQ", [ "Basics" ] )
    , ( "GT", [ "Basics" ] )
    , ( "True", [ "Basics" ] )
    , ( "False", [ "Basics" ] )
    , ( "identity", [ "Basics" ] )
    , ( "always", [ "Basics" ] )
    , ( "not", [ "Basics" ] )
    , ( "compare", [ "Basics" ] )
    , ( "(==)", [ "Basics" ] )
    , ( "(/=)", [ "Basics" ] )
    , ( "(&&)", [ "Basics" ] )
    , ( "(||)", [ "Basics" ] )
    , ( "(<)", [ "Basics" ] )
    , ( "(>)", [ "Basics" ] )
    , ( "(<=)", [ "Basics" ] )
    , ( "(>=)", [ "Basics" ] )
    , ( "(++)", [ "Basics" ] )
    , ( "(+)", [ "Basics" ] )
    , ( "(-)", [ "Basics" ] )
    , ( "(*)", [ "Basics" ] )
    , ( "(//)", [ "Basics" ] )
    , ( "(^)", [ "Basics" ] )
    , ( "(|>)", [ "Basics" ] )
    , ( "(<|)", [ "Basics" ] )
    , ( "(>>)", [ "Basics" ] )
    , ( "(<<)", [ "Basics" ] )
    , ( "min", [ "Basics" ] )
    , ( "max", [ "Basics" ] )
    , ( "modBy", [ "Basics" ] )
    , ( "remainderBy", [ "Basics" ] )
    , ( "negate", [ "Basics" ] )
    , ( "abs", [ "Basics" ] )
    , ( "clamp", [ "Basics" ] )
    , ( "(::)", [ "List" ] )
    , ( "Nothing", [ "Maybe" ] )
    , ( "Just", [ "Maybe" ] )
    , ( "Err", [ "Result" ] )
    , ( "Ok", [ "Result" ] )
    ]
        |> Dict.fromList


elmDeclarationsToExposeToGlobalDefaultQualifiedNames : Set.Set String
elmDeclarationsToExposeToGlobalDefaultQualifiedNames =
    elmValuesToExposeToGlobalDefault
        |> Dict.toList
        |> List.map
            (\( name, moduleName ) ->
                String.join "." (List.concat [ moduleName, [ name ] ])
            )
        |> Set.fromList


elmDeclarationsOverrides : Dict.Dict (List String) (Dict.Dict String Pine.Value)
elmDeclarationsOverrides =
    [ ( [ "Basics" ]
      , [ ( "True"
          , Pine.trueValue
          )
        , ( "False"
          , Pine.falseValue
          )
        ]
            |> Dict.fromList
      )
    ]
        |> Dict.fromList


elmDeclarationsOverridesExpressions : Dict.Dict String Expression
elmDeclarationsOverridesExpressions =
    elmDeclarationsOverrides
        |> Dict.toList
        |> List.concatMap
            (\( moduleName, declarations ) ->
                declarations
                    |> Dict.toList
                    |> List.concatMap
                        (\( declarationName, declarationValue ) ->
                            [ ( declarationName
                              , LiteralExpression declarationValue
                              )
                            , ( String.join "." (List.concat [ moduleName, [ declarationName ] ])
                              , LiteralExpression declarationValue
                              )
                            ]
                        )
            )
        |> Dict.fromList


expandElmInteractiveEnvironmentWithModules :
    Pine.Value
    -> List ProjectParsedElmFile
    -> Result String { addedModules : List ( List String, Pine.Value ), environment : Pine.Value }
expandElmInteractiveEnvironmentWithModules environmentBefore newParsedElmModules =
    case
        Common.resultListMapCombine
            (\file ->
                case
                    listModuleTransitiveDependencies
                        (List.map .parsedModule newParsedElmModules)
                        file.parsedModule
                of
                    Err error ->
                        Err ( file, error )

                    Ok moduleNames ->
                        Ok ( file, moduleNames )
            )
            newParsedElmModules
    of
        Err ( file, error ) ->
            Err
                ("Failed to resolve dependencies for module "
                    ++ String.join "."
                        (Elm.Syntax.Module.moduleName
                            (Elm.Syntax.Node.value file.parsedModule.moduleDefinition)
                        )
                    ++ ": "
                    ++ error
                )

        Ok modulesWithDependencies ->
            let
                modulesOrderedByDeps : List ( ProjectParsedElmFile, List Elm.Syntax.ModuleName.ModuleName )
                modulesOrderedByDeps =
                    List.sortBy
                        (\( _, dependencies ) ->
                            List.length dependencies
                        )
                        modulesWithDependencies
            in
            expandEnvWithModulesOrdered
                environmentBefore
                (List.map Tuple.first modulesOrderedByDeps)


expandEnvWithModulesOrdered :
    Pine.Value
    -> List ProjectParsedElmFile
    -> Result String { addedModules : List ( List String, Pine.Value ), environment : Pine.Value }
expandEnvWithModulesOrdered environmentBefore newParsedElmModules =
    case getDeclarationsFromEnvironment environmentBefore of
        Err error ->
            Err ("Failed to get declarations from environment: " ++ error)

        Ok environmentBeforeDeclarations ->
            case separateEnvironmentDeclarations environmentBeforeDeclarations of
                Err err ->
                    Err ("Failed to separate declarations from environment: " ++ err)

                Ok separateEnvironmentDeclarationsBefore ->
                    case
                        expandEnvWithModulesRecursive
                            (Dict.map
                                (\_ ( _, moduleParsed ) ->
                                    moduleParsed
                                )
                                separateEnvironmentDeclarationsBefore.modules
                            )
                            newParsedElmModules
                    of
                        Err error ->
                            Err error

                        Ok compiledModules ->
                            let
                                modulesValues : List ( List String, Pine.Value )
                                modulesValues =
                                    Dict.foldl
                                        (\moduleName moduleStruct aggregate ->
                                            let
                                                moduleValue =
                                                    emitModuleValue moduleStruct
                                            in
                                            ( moduleName, moduleValue )
                                                :: aggregate
                                        )
                                        []
                                        compiledModules

                                modulesValuesWithFlatNames : List ( String, Pine.Value )
                                modulesValuesWithFlatNames =
                                    List.map
                                        (\( moduleName, moduleValue ) ->
                                            ( String.join "." moduleName, moduleValue )
                                        )
                                        modulesValues
                            in
                            Ok
                                { addedModules = modulesValues
                                , environment = Pine.environmentFromDeclarations modulesValuesWithFlatNames
                                }


expandEnvWithModulesRecursive :
    Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    -> List ProjectParsedElmFile
    -> Result String (Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation)
expandEnvWithModulesRecursive environmentBefore parsedElmModules =
    case parsedElmModules of
        [] ->
            Ok environmentBefore

        moduleToTranslate :: followingModules ->
            let
                (Elm.Syntax.Node.Node _ moduleToTranslateDefinition) =
                    moduleToTranslate.parsedModule.moduleDefinition

                moduleName =
                    Elm.Syntax.Module.moduleName moduleToTranslateDefinition
            in
            case
                compileElmModuleIntoNamedExports
                    environmentBefore
                    moduleToTranslate
            of
                Err error ->
                    Err
                        ("Failed to compile elm module '"
                            ++ String.join "." moduleName
                            ++ "': "
                            ++ error
                        )

                Ok ( _, moduleValue ) ->
                    let
                        nextEnvironment : Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
                        nextEnvironment =
                            Dict.insert moduleName moduleValue environmentBefore
                    in
                    expandEnvWithModulesRecursive
                        nextEnvironment
                        followingModules


compileElmModuleIntoNamedExports :
    Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    -> ProjectParsedElmFile
    -> Result String ( Elm.Syntax.ModuleName.ModuleName, ElmModuleInCompilation )
compileElmModuleIntoNamedExports availableModules moduleToTranslate =
    let
        (Elm.Syntax.Node.Node _ moduleDefSyntax) =
            moduleToTranslate.parsedModule.moduleDefinition

        moduleName =
            Elm.Syntax.Module.moduleName moduleDefSyntax

        moduleAliases : Dict.Dict (List String) (List String)
        moduleAliases =
            List.foldl
                (\(Elm.Syntax.Node.Node _ importSyntax) dict ->
                    case importSyntax.moduleAlias of
                        Nothing ->
                            dict

                        Just (Elm.Syntax.Node.Node _ moduleAlias) ->
                            let
                                (Elm.Syntax.Node.Node _ canonicalModuleName) =
                                    importSyntax.moduleName
                            in
                            Dict.insert moduleAlias canonicalModuleName dict
                )
                Dict.empty
                moduleToTranslate.parsedModule.imports

        parsedImports : List ModuleImportStatement
        parsedImports =
            List.map
                (\(Elm.Syntax.Node.Node _ imp) -> parseElmSyntaxImport imp)
                moduleToTranslate.parsedModule.imports

        localTypeDeclarations : Dict.Dict String ElmModuleTypeDeclaration
        localTypeDeclarations =
            List.foldl
                (\(Elm.Syntax.Node.Node _ declaration) aggregate ->
                    case declaration of
                        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                            let
                                (Elm.Syntax.Node.Node _ declName) =
                                    choiceTypeDeclaration.name
                            in
                            Dict.insert
                                declName
                                (ElmModuleChoiceTypeDeclaration
                                    { tags =
                                        choiceTypeDeclaration.constructors
                                            |> List.filter
                                                (Elm.Syntax.Node.value
                                                    >> .name
                                                    >> Elm.Syntax.Node.value
                                                    >> Dict.get
                                                    >> (|>) elmDeclarationsOverridesExpressions
                                                    >> (==) Nothing
                                                )
                                            |> List.foldl
                                                (\(Elm.Syntax.Node.Node _ valueConstructor) ->
                                                    let
                                                        (Elm.Syntax.Node.Node _ valueConstructorName) =
                                                            valueConstructor.name
                                                    in
                                                    Dict.insert
                                                        valueConstructorName
                                                        { argumentsCount = List.length valueConstructor.arguments
                                                        }
                                                )
                                                Dict.empty
                                    }
                                )
                                aggregate

                        Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
                            let
                                (Elm.Syntax.Node.Node _ declName) =
                                    aliasDeclaration.name
                            in
                            case aliasDeclaration.typeAnnotation of
                                Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.Record record) ->
                                    Dict.insert
                                        declName
                                        (ElmModuleRecordTypeDeclaration
                                            (List.map
                                                (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ recordField, _ )) ->
                                                    recordField
                                                )
                                                record
                                            )
                                        )
                                        aggregate

                                Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.GenericRecord _ (Elm.Syntax.Node.Node _ record)) ->
                                    Dict.insert
                                        declName
                                        (ElmModuleRecordTypeDeclaration
                                            (List.map
                                                (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ recordField, _ )) ->
                                                    recordField
                                                )
                                                record
                                            )
                                        )
                                        aggregate

                                _ ->
                                    aggregate

                        _ ->
                            aggregate
                )
                Dict.empty
                moduleToTranslate.parsedModule.declarations

        ( compilationStackForImport, initialEmitStack ) =
            compilationAndEmitStackFromModulesInCompilation
                availableModules
                { moduleAliases = moduleAliases
                , parsedImports = parsedImports
                , localTypeDeclarations = localTypeDeclarations
                }

        initialCompilationStack =
            { compilationStackForImport
                | elmValuesToExposeToGlobal =
                    Dict.filter (\_ originModule -> not (originModule == moduleName))
                        compilationStackForImport.elmValuesToExposeToGlobal
            }

        moduleExposingList : Elm.Syntax.Exposing.Exposing
        moduleExposingList =
            Elm.Syntax.Module.exposingList moduleDefSyntax

        redirectsForInfix : Dict.Dict String String
        redirectsForInfix =
            List.foldl
                (\(Elm.Syntax.Node.Node _ declaration) aggregate ->
                    case declaration of
                        Elm.Syntax.Declaration.FunctionDeclaration _ ->
                            aggregate

                        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                            aggregate

                        Elm.Syntax.Declaration.InfixDeclaration infixDeclaration ->
                            let
                                (Elm.Syntax.Node.Node _ operator) =
                                    infixDeclaration.operator

                                (Elm.Syntax.Node.Node _ function) =
                                    infixDeclaration.function
                            in
                            Dict.insert
                                ("(" ++ operator ++ ")")
                                function
                                aggregate

                        _ ->
                            aggregate
                )
                Dict.empty
                moduleToTranslate.parsedModule.declarations

        functionsToExposeForInfix : Set.Set String
        functionsToExposeForInfix =
            Dict.foldl (\_ function aggregate -> Set.insert function aggregate)
                Set.empty
                redirectsForInfix

        localFunctionDeclarations : Dict.Dict String Elm.Syntax.Expression.Function
        localFunctionDeclarations =
            moduleToTranslate.parsedModule.declarations
                |> List.foldl
                    (\(Elm.Syntax.Node.Node _ declaration) aggregate ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                let
                                    (Elm.Syntax.Node.Node _ function) =
                                        functionDeclaration.declaration

                                    (Elm.Syntax.Node.Node _ name) =
                                        function.name
                                in
                                Dict.insert name functionDeclaration aggregate

                            _ ->
                                aggregate
                    )
                    Dict.empty

        exposedFunctionDecls : Set.Set String
        exposedFunctionDecls =
            Set.union
                functionsToExposeForInfix
                (case moduleExposingList of
                    Elm.Syntax.Exposing.All _ ->
                        Dict.foldl
                            (\name _ aggregate ->
                                Set.insert name aggregate
                            )
                            Set.empty
                            localFunctionDeclarations

                    Elm.Syntax.Exposing.Explicit explicitList ->
                        List.foldl
                            (\(Elm.Syntax.Node.Node _ item) aggregate ->
                                case item of
                                    Elm.Syntax.Exposing.FunctionExpose name ->
                                        Set.insert name aggregate

                                    _ ->
                                        aggregate
                            )
                            Set.empty
                            explicitList
                )

        localFunctionsResult : Result String (List ( String, Pine.Value ))
        localFunctionsResult =
            case
                Common.resultDictMapCombine
                    (\functionName functionDeclaration ->
                        case compileElmSyntaxFunction initialCompilationStack functionDeclaration of
                            Err err ->
                                Err ("Failed to compile function '" ++ functionName ++ "': " ++ err)

                            Ok ( _, compiledFunction ) ->
                                Ok compiledFunction
                    )
                    localFunctionDeclarations
            of
                Err err ->
                    Err err

                Ok localFunctionDeclarationsCompiled ->
                    emitModuleFunctionDeclarations
                        initialEmitStack
                        { exposedDeclarations =
                            Dict.filter (\declName _ -> Set.member declName exposedFunctionDecls)
                                localFunctionDeclarationsCompiled
                        , supportingDeclarations =
                            localFunctionDeclarationsCompiled
                        }
    in
    case localFunctionsResult of
        Err error ->
            Err ("Failed to compile declaration: " ++ error)

        Ok functionDeclarations ->
            let
                declarationsValuesForInfix =
                    Dict.foldl
                        (\name function aggregate ->
                            case Common.listFind (\( declName, _ ) -> declName == function) functionDeclarations of
                                Nothing ->
                                    aggregate

                                Just ( _, value ) ->
                                    ( name, value ) :: aggregate
                        )
                        []
                        redirectsForInfix

                exportedFuncsLessInfix : Dict.Dict String Pine.Value
                exportedFuncsLessInfix =
                    List.foldl
                        (\( declName, declValue ) ->
                            if Set.member declName exposedFunctionDecls then
                                Dict.insert declName declValue

                            else
                                identity
                        )
                        Dict.empty
                        functionDeclarations

                exportedFunctionDeclarations : Dict.Dict String Pine.Value
                exportedFunctionDeclarations =
                    List.foldl
                        (\( declName, declValue ) -> Dict.insert declName declValue)
                        exportedFuncsLessInfix
                        declarationsValuesForInfix
            in
            Ok
                ( moduleName
                , { functionDeclarations = exportedFunctionDeclarations
                  , typeDeclarations = localTypeDeclarations
                  }
                )


parseElmSyntaxImport : Elm.Syntax.Import.Import -> ModuleImportStatement
parseElmSyntaxImport importSyntax =
    let
        (Elm.Syntax.Node.Node _ canonicalModuleName) =
            importSyntax.moduleName

        localModuleName =
            case importSyntax.moduleAlias of
                Nothing ->
                    canonicalModuleName

                Just (Elm.Syntax.Node.Node _ moduleAlias) ->
                    moduleAlias

        exposedNamesFromTopLevelItem : Elm.Syntax.Exposing.TopLevelExpose -> ModuleImportTopLevelExpose
        exposedNamesFromTopLevelItem topLevelItem =
            case topLevelItem of
                Elm.Syntax.Exposing.InfixExpose infixExpose ->
                    { name = infixExpose
                    , open = False
                    }

                Elm.Syntax.Exposing.FunctionExpose functionExpose ->
                    { name = functionExpose
                    , open = False
                    }

                Elm.Syntax.Exposing.TypeOrAliasExpose typeOrAlias ->
                    { name = typeOrAlias
                    , open = False
                    }

                Elm.Syntax.Exposing.TypeExpose typeExpose ->
                    { name = typeExpose.name
                    , open = typeExpose.open /= Nothing
                    }

        exposingList =
            case importSyntax.exposingList of
                Nothing ->
                    Nothing

                Just (Elm.Syntax.Node.Node _ (Elm.Syntax.Exposing.All _)) ->
                    Just ExposingAll

                Just (Elm.Syntax.Node.Node _ (Elm.Syntax.Exposing.Explicit topLevelList)) ->
                    Just
                        (ExposingSelected
                            (List.map (\(Elm.Syntax.Node.Node _ item) -> exposedNamesFromTopLevelItem item)
                                topLevelList
                            )
                        )
    in
    { canonicalModuleName = canonicalModuleName
    , localModuleName = localModuleName
    , exposingList = exposingList
    }


compilationAndEmitStackFromModulesInCompilation :
    Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    ->
        { moduleAliases : Dict.Dict (List String) (List String)
        , parsedImports : List ModuleImportStatement
        , localTypeDeclarations : Dict.Dict String ElmModuleTypeDeclaration
        }
    -> ( CompilationStack, EmitStack )
compilationAndEmitStackFromModulesInCompilation availableModules { moduleAliases, parsedImports, localTypeDeclarations } =
    let
        compilationStackForImport =
            { moduleAliases = moduleAliases
            , availableModules = availableModules
            , inlineableDeclarations = Dict.empty
            , elmValuesToExposeToGlobal = elmValuesToExposeToGlobalDefault
            , localTypeDeclarations = localTypeDeclarations
            , depth = 0
            }

        moduleImports =
            moduleImportsFromCompilationStack
                parsedImports
                compilationStackForImport

        mergedImports : ModuleImportsMerged
        mergedImports =
            Dict.foldl
                mergeModuleImports
                { modulesDeclarationsFlat = Dict.empty
                , choiceTypeTagConstructorDeclarations = Dict.empty
                , recordConstructorsFieldsNames = Dict.empty
                }
                moduleImports.importedModules

        importedChoiceTypeTagConstructorDeclarations : Dict.Dict String { argumentsCount : Int }
        importedChoiceTypeTagConstructorDeclarations =
            Dict.foldl
                (\_ typeDeclaration ->
                    case typeDeclaration of
                        ElmModuleChoiceTypeDeclaration choiceTypeDeclaration ->
                            Dict.union choiceTypeDeclaration.tags

                        _ ->
                            identity
                )
                mergedImports.choiceTypeTagConstructorDeclarations
                moduleImports.importedTypes

        localTypeDeclarationsSeparate :
            { choiceTypeTagDeclarations : Dict.Dict String { argumentsCount : Int }
            , recordTypeDeclarations : Dict.Dict String (List String)
            }
        localTypeDeclarationsSeparate =
            localTypeDeclarations
                |> Dict.foldl
                    (\typeName typeDeclaration aggregate ->
                        case typeDeclaration of
                            ElmModuleChoiceTypeDeclaration choiceTypeDeclaration ->
                                { aggregate
                                    | choiceTypeTagDeclarations =
                                        Dict.union
                                            (Dict.map
                                                (\_ tag -> { argumentsCount = tag.argumentsCount })
                                                choiceTypeDeclaration.tags
                                            )
                                            aggregate.choiceTypeTagDeclarations
                                }

                            ElmModuleRecordTypeDeclaration fields ->
                                { aggregate
                                    | recordTypeDeclarations =
                                        Dict.insert typeName fields aggregate.recordTypeDeclarations
                                }
                    )
                    { recordTypeDeclarations = Dict.empty
                    , choiceTypeTagDeclarations = Dict.empty
                    }

        declarationsFromTypeAliasesFieldsNames =
            Dict.union
                localTypeDeclarationsSeparate.recordTypeDeclarations
                mergedImports.recordConstructorsFieldsNames

        choiceTypeTagConstructorDeclarations =
            Dict.union
                localTypeDeclarationsSeparate.choiceTypeTagDeclarations
                importedChoiceTypeTagConstructorDeclarations

        declarationsFromTypeAliases : Dict.Dict String (List Expression -> Expression)
        declarationsFromTypeAliases =
            Dict.map (\_ -> compileElmRecordConstructor)
                declarationsFromTypeAliasesFieldsNames

        declarationsFromChoiceTypes : Dict.Dict String (List Expression -> Expression)
        declarationsFromChoiceTypes =
            Dict.map
                (\tagName { argumentsCount } ->
                    compileElmChoiceTypeTagConstructor
                        { tagName =
                            Maybe.withDefault tagName (List.head (List.reverse (String.split "." tagName)))
                        , argumentsCount = argumentsCount
                        }
                )
                choiceTypeTagConstructorDeclarations

        compilationStack =
            { compilationStackForImport
                | inlineableDeclarations =
                    Dict.union
                        declarationsFromTypeAliases
                        declarationsFromChoiceTypes
            }

        importedFunctionsBeforeParse : Dict.Dict String Pine.Value
        importedFunctionsBeforeParse =
            Dict.union
                mergedImports.modulesDeclarationsFlat
                moduleImports.importedFunctions

        importedFunctions : Dict.Dict String ( FirCompiler.EnvironmentFunctionEntry, Pine.Value )
        importedFunctions =
            Dict.map
                (\functionName functionValue ->
                    let
                        ( paramCount, expectedEnv ) =
                            case FirCompiler.parseFunctionRecordFromValueTagged functionValue of
                                Err _ ->
                                    ( 0
                                    , FirCompiler.LocalEnvironment { expectedDecls = [] }
                                    )

                                Ok functionRecord ->
                                    ( functionRecord.parameterCount
                                    , FirCompiler.ImportedEnvironment
                                        { pathToRecordFromEnvEntry = [] }
                                    )
                    in
                    ( { functionName = functionName
                      , parameterCount = paramCount
                      , expectedEnvironment = expectedEnv
                      }
                    , functionValue
                    )
                )
                importedFunctionsBeforeParse

        emitStack =
            { importedFunctions = importedFunctions
            , declarationsDependencies = Dict.empty
            , environmentFunctions = []
            , environmentDeconstructions = Dict.empty
            }
    in
    ( compilationStack
    , emitStack
    )


mergeModuleImports : List String -> ElmModuleInCompilation -> ModuleImportsMerged -> ModuleImportsMerged
mergeModuleImports importedModuleName importedModule aggregateImports =
    let
        moduleNameFlat =
            String.join "." importedModuleName

        modulesDeclarationsFlat : Dict.Dict String Pine.Value
        modulesDeclarationsFlat =
            Dict.foldl
                (\declName ->
                    Dict.insert (String.join "." [ moduleNameFlat, declName ])
                )
                aggregateImports.modulesDeclarationsFlat
                importedModule.functionDeclarations

        typeDecls :
            { choiceTypeTags : Dict.Dict String { argumentsCount : Int }
            , recordConstructors : Dict.Dict String (List String)
            }
        typeDecls =
            Dict.foldl
                (\typeName typeDeclaration aggregate ->
                    case typeDeclaration of
                        ElmModuleChoiceTypeDeclaration choiceTypeDeclaration ->
                            { aggregate
                                | choiceTypeTags =
                                    Dict.foldl
                                        (\tagName tag innerAggregate ->
                                            let
                                                qualifiedName =
                                                    String.join "." [ moduleNameFlat, tagName ]

                                                isAutoImported =
                                                    Set.member
                                                        qualifiedName
                                                        elmDeclarationsToExposeToGlobalDefaultQualifiedNames

                                                withQualifiedName =
                                                    Dict.insert
                                                        qualifiedName
                                                        { argumentsCount = tag.argumentsCount }
                                                        innerAggregate
                                            in
                                            if isAutoImported then
                                                Dict.insert
                                                    tagName
                                                    { argumentsCount = tag.argumentsCount }
                                                    withQualifiedName

                                            else
                                                withQualifiedName
                                        )
                                        aggregate.choiceTypeTags
                                        choiceTypeDeclaration.tags
                            }

                        ElmModuleRecordTypeDeclaration fields ->
                            { aggregate
                                | recordConstructors =
                                    Dict.insert
                                        (String.join "." [ moduleNameFlat, typeName ])
                                        fields
                                        aggregate.recordConstructors
                            }
                )
                { choiceTypeTags = aggregateImports.choiceTypeTagConstructorDeclarations
                , recordConstructors = aggregateImports.recordConstructorsFieldsNames
                }
                importedModule.typeDeclarations
    in
    { aggregateImports
        | modulesDeclarationsFlat = modulesDeclarationsFlat
        , choiceTypeTagConstructorDeclarations = typeDecls.choiceTypeTags
        , recordConstructorsFieldsNames = typeDecls.recordConstructors
    }


moduleImportsFromCompilationStack :
    List ModuleImportStatement
    -> CompilationStack
    -> ModuleImports
moduleImportsFromCompilationStack explicitImports compilation =
    let
        importedModulesImplicit : Dict.Dict (List String) ElmModuleInCompilation
        importedModulesImplicit =
            Dict.filter
                (\moduleName _ -> List.member moduleName autoImportedModulesNames)
                compilation.availableModules

        functionsFromImportStatement :
            ModuleImportStatement
            -> Maybe ( ElmModuleInCompilation, Dict.Dict String Pine.Value )
        functionsFromImportStatement explicitImport =
            case Dict.get explicitImport.canonicalModuleName compilation.availableModules of
                Nothing ->
                    Nothing

                Just availableModule ->
                    let
                        exposedDeclarations : Dict.Dict String Pine.Value
                        exposedDeclarations =
                            case explicitImport.exposingList of
                                Nothing ->
                                    Dict.empty

                                Just ExposingAll ->
                                    availableModule.functionDeclarations

                                Just (ExposingSelected exposedNames) ->
                                    List.foldl
                                        (\exposedName ->
                                            case Dict.get exposedName.name availableModule.functionDeclarations of
                                                Nothing ->
                                                    identity

                                                Just functionDeclaration ->
                                                    Dict.insert exposedName.name functionDeclaration
                                        )
                                        Dict.empty
                                        exposedNames
                    in
                    Just
                        ( availableModule
                        , exposedDeclarations
                        )

        typesFromImportStatement : ModuleImportStatement -> Maybe (Dict.Dict String ElmModuleTypeDeclaration)
        typesFromImportStatement explicitImport =
            case Dict.get explicitImport.canonicalModuleName compilation.availableModules of
                Nothing ->
                    Nothing

                Just availableModule ->
                    let
                        exposedDeclarations : Dict.Dict String ElmModuleTypeDeclaration
                        exposedDeclarations =
                            case explicitImport.exposingList of
                                Nothing ->
                                    Dict.empty

                                Just ExposingAll ->
                                    availableModule.typeDeclarations

                                Just (ExposingSelected exposedNames) ->
                                    List.foldl
                                        (\topLevelExpose ->
                                            case Dict.get topLevelExpose.name availableModule.typeDeclarations of
                                                Nothing ->
                                                    identity

                                                Just typeDeclaration ->
                                                    Dict.insert topLevelExpose.name
                                                        (mapTypeDeclarationForImport topLevelExpose typeDeclaration)
                                        )
                                        Dict.empty
                                        exposedNames
                    in
                    Just exposedDeclarations

        parsedExplicitImports : List ( List String, ( ElmModuleInCompilation, Dict.Dict String Pine.Value ) )
        parsedExplicitImports =
            List.foldl
                (\explicitImport aggregate ->
                    case functionsFromImportStatement explicitImport of
                        Nothing ->
                            aggregate

                        Just match ->
                            ( explicitImport.localModuleName
                            , match
                            )
                                :: aggregate
                )
                []
                explicitImports

        importedFunctions : Dict.Dict String Pine.Value
        importedFunctions =
            Dict.union
                (Dict.foldl
                    (\name moduleName aggregate ->
                        case Dict.get moduleName compilation.availableModules of
                            Nothing ->
                                aggregate

                            Just moduleInCompilation ->
                                case Dict.get name moduleInCompilation.functionDeclarations of
                                    Nothing ->
                                        aggregate

                                    Just value ->
                                        Dict.insert name value aggregate
                    )
                    Dict.empty
                    compilation.elmValuesToExposeToGlobal
                )
                (List.foldl
                    (\( _, ( _, functions ) ) aggregate ->
                        Dict.union functions aggregate
                    )
                    Dict.empty
                    parsedExplicitImports
                )

        importedModules : Dict.Dict (List String) ElmModuleInCompilation
        importedModules =
            parsedExplicitImports
                |> List.map (Tuple.mapSecond Tuple.first)
                |> Dict.fromList
                |> Dict.union importedModulesImplicit

        importedTypes : Dict.Dict String ElmModuleTypeDeclaration
        importedTypes =
            List.foldl
                (\explicitImport aggregate ->
                    case typesFromImportStatement explicitImport of
                        Nothing ->
                            aggregate

                        Just types ->
                            Dict.union types aggregate
                )
                Dict.empty
                explicitImports
    in
    { importedModules = importedModules
    , importedFunctions = importedFunctions
    , importedTypes = importedTypes
    }


mapTypeDeclarationForImport : { a | open : Bool } -> ElmModuleTypeDeclaration -> ElmModuleTypeDeclaration
mapTypeDeclarationForImport { open } typeDeclaration =
    case typeDeclaration of
        ElmModuleRecordTypeDeclaration _ ->
            typeDeclaration

        ElmModuleChoiceTypeDeclaration choiceTypeDeclaration ->
            if open then
                typeDeclaration

            else
                ElmModuleChoiceTypeDeclaration
                    { choiceTypeDeclaration | tags = Dict.empty }


compileElmSyntaxExpression :
    CompilationStack
    -> Elm.Syntax.Expression.Expression
    -> Result String Expression
compileElmSyntaxExpression stackBefore elmExpression =
    let
        stack =
            { stackBefore
                | depth = stackBefore.depth + 1
            }
    in
    case elmExpression of
        Elm.Syntax.Expression.Literal literal ->
            Ok (LiteralExpression (valueFromString literal))

        Elm.Syntax.Expression.CharLiteral char ->
            Ok (LiteralExpression (Pine.valueFromChar char))

        Elm.Syntax.Expression.Integer integer ->
            Ok (LiteralExpression (Pine.valueFromInt integer))

        Elm.Syntax.Expression.Hex integer ->
            Ok (LiteralExpression (Pine.valueFromInt integer))

        Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node _ negatedElmExpression) ->
            case compileElmSyntaxExpression stack negatedElmExpression of
                Err error ->
                    Err ("Failed to compile negated expression: " ++ error)

                Ok negatedExpression ->
                    Ok
                        (KernelApplicationExpression
                            { functionName = "negate"
                            , argument = negatedExpression
                            }
                        )

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            compileElmFunctionOrValueLookup ( moduleName, localName ) stack

        Elm.Syntax.Expression.Application application ->
            case List.map Elm.Syntax.Node.value application of
                [] ->
                    Err "Invalid shape of application: Zero elements in the list"

                appliedFunctionElmSyntax :: argumentsElmSyntax ->
                    compileElmSyntaxApplication stack appliedFunctionElmSyntax argumentsElmSyntax

        Elm.Syntax.Expression.OperatorApplication operator _ (Elm.Syntax.Node.Node _ leftExpr) (Elm.Syntax.Node.Node _ rightExpr) ->
            let
                orderedElmExpression =
                    mapExpressionForOperatorPrecedence elmExpression
            in
            if orderedElmExpression /= elmExpression then
                compileElmSyntaxExpression stack orderedElmExpression

            else
                case searchCompileElmSyntaxOperatorOptimized stack operator leftExpr rightExpr of
                    Just result ->
                        result

                    Nothing ->
                        case compileElmSyntaxExpression stack leftExpr of
                            Err err ->
                                Err ("Failed to compile left expression of OperatorApplication: " ++ err)

                            Ok leftExpression ->
                                case compileElmSyntaxExpression stack rightExpr of
                                    Err err ->
                                        Err ("Failed to compile right expression of OperatorApplication: " ++ err)

                                    Ok rightExpression ->
                                        case
                                            compileElmFunctionOrValueLookup
                                                ( [], "(" ++ operator ++ ")" )
                                                stack
                                        of
                                            Err err ->
                                                Err ("Failed to compile operator: " ++ err)

                                            Ok operationFunction ->
                                                Ok
                                                    (FunctionApplicationExpression
                                                        operationFunction
                                                        [ leftExpression, rightExpression ]
                                                    )

        Elm.Syntax.Expression.PrefixOperator operator ->
            compileElmFunctionOrValueLookup ( [], "(" ++ operator ++ ")" ) stack

        Elm.Syntax.Expression.IfBlock (Elm.Syntax.Node.Node _ elmCondition) (Elm.Syntax.Node.Node _ elmExpressionIfTrue) (Elm.Syntax.Node.Node _ elmExpressionIfFalse) ->
            case compileElmSyntaxExpression stack elmCondition of
                Err error ->
                    Err ("Failed to compile Elm condition: " ++ error)

                Ok conditionExpression ->
                    case compileElmSyntaxExpression stack elmExpressionIfTrue of
                        Err error ->
                            Err ("Failed to compile Elm expressionIfTrue: " ++ error)

                        Ok expressionIfTrue ->
                            case compileElmSyntaxExpression stack elmExpressionIfFalse of
                                Err error ->
                                    Err ("Failed to compile Elm expressionIfFalse: " ++ error)

                                Ok expressionIfFalse ->
                                    Ok
                                        (ConditionalExpression
                                            { condition = conditionExpression
                                            , ifTrue = expressionIfTrue
                                            , ifFalse = expressionIfFalse
                                            }
                                        )

        Elm.Syntax.Expression.LetExpression letBlock ->
            compileElmSyntaxLetBlock stack letBlock

        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ parenthesizedExpression) ->
            compileElmSyntaxExpression stack parenthesizedExpression

        Elm.Syntax.Expression.ListExpr listExpression ->
            case
                Common.resultListMapCombine
                    (\(Elm.Syntax.Node.Node _ listItem) -> compileElmSyntaxExpression stack listItem)
                    listExpression
            of
                Err err ->
                    Err err

                Ok expressions ->
                    Ok (ListExpression expressions)

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            compileElmSyntaxCaseBlock stack caseBlock

        Elm.Syntax.Expression.LambdaExpression lambdaExpression ->
            compileElmSyntaxLambda stack lambdaExpression

        Elm.Syntax.Expression.RecordExpr recordExpr ->
            compileElmSyntaxRecord
                stack
                (List.map Elm.Syntax.Node.value recordExpr)

        Elm.Syntax.Expression.TupledExpression tupleElements ->
            case
                Common.resultListMapCombine
                    (\(Elm.Syntax.Node.Node _ listItem) -> compileElmSyntaxExpression stack listItem)
                    tupleElements
            of
                Err err ->
                    Err err

                Ok expressions ->
                    Ok (ListExpression expressions)

        Elm.Syntax.Expression.RecordAccess (Elm.Syntax.Node.Node _ recordExpr) (Elm.Syntax.Node.Node _ fieldName) ->
            compileElmSyntaxRecordAccess
                stack
                fieldName
                recordExpr

        Elm.Syntax.Expression.RecordAccessFunction accessSyntax ->
            let
                fieldName =
                    if String.startsWith "." accessSyntax then
                        String.dropLeft 1 accessSyntax

                    else
                        accessSyntax
            in
            Ok (compileElmSyntaxRecordAccessFunction fieldName)

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ recordNameSyntax) settersNodes ->
            compileElmSyntaxRecordUpdate
                stack
                (List.map
                    (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, Elm.Syntax.Node.Node _ valueExpr )) ->
                        ( fieldName, valueExpr )
                    )
                    settersNodes
                )
                recordNameSyntax

        Elm.Syntax.Expression.UnitExpr ->
            Ok (ListExpression [])

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "Unsupported type of expression: GLSLExpression"

        Elm.Syntax.Expression.Floatable _ ->
            Err "Unsupported type of expression: Floatable"

        Elm.Syntax.Expression.Operator operator ->
            Err ("Unsupported type of expression: Operator: " ++ operator)


compileElmSyntaxApplication :
    CompilationStack
    -> Elm.Syntax.Expression.Expression
    -> List Elm.Syntax.Expression.Expression
    -> Result String Expression
compileElmSyntaxApplication stack appliedFunctionElmSyntax argumentsElmSyntax =
    case Common.resultListMapCombine (compileElmSyntaxExpression stack) argumentsElmSyntax of
        Err error ->
            Err ("Failed to compile Elm arguments: " ++ error)

        Ok arguments ->
            let
                continueWithDefaultApplication () =
                    case compileElmSyntaxExpression stack appliedFunctionElmSyntax of
                        Err error ->
                            Err ("Failed to compile Elm function syntax: " ++ error)

                        Ok appliedFunctionSyntax ->
                            Ok
                                (FunctionApplicationExpression
                                    appliedFunctionSyntax
                                    arguments
                                )
            in
            case appliedFunctionElmSyntax of
                Elm.Syntax.Expression.FunctionOrValue functionModuleName functionLocalName ->
                    let
                        continueWithDefaultNamedApplication () =
                            let
                                functionFlatName =
                                    String.join "." (List.concat [ functionModuleName, [ functionLocalName ] ])
                            in
                            case Dict.get functionFlatName elmDeclarationsOverridesExpressions of
                                Just declarationOverride ->
                                    Ok declarationOverride

                                Nothing ->
                                    case Dict.get functionFlatName stack.inlineableDeclarations of
                                        Just applicableDeclaration ->
                                            Ok (applicableDeclaration arguments)

                                        _ ->
                                            continueWithDefaultApplication ()
                    in
                    case functionModuleName of
                        [ "Pine_kernel" ] ->
                            case arguments of
                                [ singleArgumentExpression ] ->
                                    Ok
                                        (KernelApplicationExpression
                                            { functionName = functionLocalName
                                            , argument = singleArgumentExpression
                                            }
                                        )

                                _ ->
                                    Err "Invalid argument list for kernel application: Wrap arguments into a single list expression"

                        [ "Debug" ] ->
                            case functionLocalName of
                                "log" ->
                                    case arguments of
                                        [ _, contentArg ] ->
                                            let
                                                stringTag =
                                                    "Elm application of Debug.log"
                                            in
                                            Ok (StringTagExpression stringTag contentArg)

                                        _ ->
                                            Err "Invalid argument list for Debug.log: Expected two arguments"

                                "toString" ->
                                    Err "Unsupported - Debug.toString is not implemented yet"

                                _ ->
                                    continueWithDefaultNamedApplication ()

                        _ ->
                            continueWithDefaultNamedApplication ()

                _ ->
                    continueWithDefaultApplication ()


compileElmSyntaxLetBlock :
    CompilationStack
    -> Elm.Syntax.Expression.LetBlock
    -> Result String Expression
compileElmSyntaxLetBlock stackBefore letBlock =
    case
        Common.resultListMapCombine
            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                case letDeclaration of
                    Elm.Syntax.Expression.LetFunction _ ->
                        Ok []

                    Elm.Syntax.Expression.LetDestructuring (Elm.Syntax.Node.Node _ pattern) (Elm.Syntax.Node.Node _ destructuredExpressionElm) ->
                        case compileElmSyntaxExpression stackBefore destructuredExpressionElm of
                            Err err ->
                                Err err

                            Ok destructuredExpression ->
                                case compileElmSyntaxPattern stackBefore pattern of
                                    Err err ->
                                        Err err

                                    Ok compiledPattern ->
                                        Ok
                                            (List.map
                                                (\( declName, deconsExpr ) ->
                                                    ( declName
                                                    , applicableDeclarationFromConstructorExpression
                                                        (expressionForDeconstructions deconsExpr destructuredExpression)
                                                    )
                                                )
                                                compiledPattern.declarations
                                            )
            )
            letBlock.declarations
    of
        Err err ->
            Err err

        Ok newAvailableDeclarations ->
            let
                inlineableDeclarations =
                    List.foldl
                        (\( declName, declExpr ) ->
                            Dict.insert declName declExpr
                        )
                        stackBefore.inlineableDeclarations
                        (List.concat newAvailableDeclarations)

                stack =
                    { stackBefore
                        | inlineableDeclarations = inlineableDeclarations
                    }
            in
            case
                Common.resultListMapCombine
                    (\(Elm.Syntax.Node.Node _ letEntry) ->
                        compileElmSyntaxLetDeclaration stack letEntry
                    )
                    letBlock.declarations
            of
                Err error ->
                    Err ("Failed to compile declaration in let block: " ++ error)

                Ok letEntries ->
                    case compileElmSyntaxExpression stack (Elm.Syntax.Node.value letBlock.expression) of
                        Err err ->
                            Err err

                        Ok expression ->
                            Ok
                                (DeclarationBlockExpression
                                    (Dict.fromList (List.concat letEntries))
                                    expression
                                )


compileElmSyntaxLetDeclaration :
    CompilationStack
    -> Elm.Syntax.Expression.LetDeclaration
    -> Result String (List ( String, Expression ))
compileElmSyntaxLetDeclaration stack declaration =
    case declaration of
        Elm.Syntax.Expression.LetFunction letFunction ->
            case compileElmSyntaxFunction stack letFunction of
                Err err ->
                    Err err

                Ok compiledFunction ->
                    Ok [ compiledFunction ]

        Elm.Syntax.Expression.LetDestructuring (Elm.Syntax.Node.Node _ patternSyntax) (Elm.Syntax.Node.Node _ exprSyntax) ->
            case compileElmSyntaxExpression stack exprSyntax of
                Err err ->
                    Err err

                Ok compiledExpression ->
                    case compileElmSyntaxPattern stack patternSyntax of
                        Err err ->
                            Err ("Failed destructuring in let block: " ++ err)

                        Ok pattern ->
                            Ok
                                (List.map
                                    (\( declName, deconsExpr ) ->
                                        ( declName
                                        , expressionForDeconstructions deconsExpr compiledExpression
                                        )
                                    )
                                    pattern.declarations
                                )


compileElmSyntaxFunction :
    CompilationStack
    -> Elm.Syntax.Expression.Function
    -> Result String ( String, Expression )
compileElmSyntaxFunction stack function =
    let
        (Elm.Syntax.Node.Node _ functionDeclaration) =
            function.declaration

        (Elm.Syntax.Node.Node _ functionDeclExpr) =
            functionDeclaration.expression

        arguments =
            List.map
                (\(Elm.Syntax.Node.Node _ arg) -> arg)
                functionDeclaration.arguments

        (Elm.Syntax.Node.Node _ functionDeclName) =
            functionDeclaration.name
    in
    case
        compileElmSyntaxFunctionWithoutName stack
            { arguments = arguments
            , expression = functionDeclExpr
            }
    of
        Err err ->
            Err err

        Ok functionWithoutName ->
            Ok
                ( functionDeclName
                , functionWithoutName
                )


compileElmSyntaxFunctionWithoutName :
    CompilationStack
    -> ElmFunctionDeclarationStruct
    -> Result String Expression
compileElmSyntaxFunctionWithoutName stackBefore function =
    case
        Common.resultListMapCombine
            (\pattern ->
                case compileElmSyntaxPattern stackBefore pattern of
                    Err err ->
                        Err err

                    Ok compiledPattern ->
                        Ok compiledPattern.declarations
            )
            function.arguments
    of
        Err error ->
            Err ("Failed to compile function parameter pattern: " ++ error)

        Ok argumentsDeconstructDeclarationsBuilders ->
            case compileElmSyntaxExpression stackBefore function.expression of
                Err err ->
                    Err err

                Ok functionBody ->
                    Ok (FunctionExpression argumentsDeconstructDeclarationsBuilders functionBody)


compileElmSyntaxLambda :
    CompilationStack
    -> Elm.Syntax.Expression.Lambda
    -> Result String Expression
compileElmSyntaxLambda stack lambda =
    let
        (Elm.Syntax.Node.Node _ lambdaExpr) =
            lambda.expression

        lambdaArgs =
            List.map
                (\(Elm.Syntax.Node.Node _ arg) -> arg)
                lambda.args
    in
    compileElmSyntaxFunctionWithoutName stack
        { arguments = lambdaArgs
        , expression = lambdaExpr
        }


compileElmSyntaxRecord :
    CompilationStack
    -> List Elm.Syntax.Expression.RecordSetter
    -> Result String Expression
compileElmSyntaxRecord stack recordSetters =
    case
        Common.resultListMapCombine
            (\( Elm.Syntax.Node.Node _ fieldName, Elm.Syntax.Node.Node _ fieldExpr ) ->
                case compileElmSyntaxExpression stack fieldExpr of
                    Err error ->
                        Err ("Failed to compile record field: " ++ error)

                    Ok fieldExpression ->
                        Ok
                            (ListExpression
                                [ LiteralExpression (Pine.valueFromString fieldName)
                                , fieldExpression
                                ]
                            )
            )
            (List.sortBy (\( Elm.Syntax.Node.Node _ fieldName, _ ) -> fieldName)
                recordSetters
            )
    of
        Err err ->
            Err err

        Ok fieldsExpressions ->
            Ok
                (ListExpression
                    [ LiteralExpression elmRecordTypeTagNameAsValue
                    , ListExpression [ ListExpression fieldsExpressions ]
                    ]
                )


compileElmSyntaxRecordAccess :
    CompilationStack
    -> String
    -> Elm.Syntax.Expression.Expression
    -> Result String Expression
compileElmSyntaxRecordAccess stack fieldName recordElmExpression =
    case compileElmSyntaxExpression stack recordElmExpression of
        Err err ->
            Err ("Failed to compile record expression: " ++ err)

        Ok recordExpression ->
            Ok (compileRecordAccessExpression fieldName recordExpression)


compileRecordAccessExpression : String -> Expression -> Expression
compileRecordAccessExpression fieldName recordExpression =
    PineFunctionApplicationExpression
        pineFunctionForRecordAccess
        (ListExpression
            [ recordExpression
            , LiteralExpression (Pine.valueFromString fieldName)
            ]
        )


compileElmSyntaxRecordAccessFunction : String -> Expression
compileElmSyntaxRecordAccessFunction fieldName =
    FunctionExpression
        [ [ ( "record-param", [] ) ] ]
        (compileRecordAccessExpression
            fieldName
            (ReferenceExpression "record-param")
        )


compileElmSyntaxRecordUpdate :
    CompilationStack
    -> List ( String, Elm.Syntax.Expression.Expression )
    -> String
    -> Result String Expression
compileElmSyntaxRecordUpdate stack setters recordName =
    case
        Common.resultListMapCombine
            (\( fieldName, fieldExpr ) ->
                case compileElmSyntaxExpression stack fieldExpr of
                    Err err ->
                        Err ("Failed to compile record update field '" ++ fieldName ++ "': " ++ err)

                    Ok compiledFieldExpr ->
                        Ok ( fieldName, compiledFieldExpr )
            )
            setters
    of
        Err error ->
            Err error

        Ok settersExpressions ->
            Ok
                (PineFunctionApplicationExpression
                    pineFunctionForRecordUpdate
                    (ListExpression
                        [ ReferenceExpression recordName
                        , ListExpression
                            (List.map
                                (\( fieldName, fieldExpr ) ->
                                    ListExpression
                                        [ LiteralExpression (Pine.valueFromString fieldName)
                                        , fieldExpr
                                        ]
                                )
                                settersExpressions
                            )
                        ]
                    )
                )


compileElmSyntaxCaseBlock :
    CompilationStack
    -> Elm.Syntax.Expression.CaseBlock
    -> Result String Expression
compileElmSyntaxCaseBlock stack caseBlock =
    let
        (Elm.Syntax.Node.Node _ caseBlockExpr) =
            caseBlock.expression
    in
    case compileElmSyntaxExpression stack caseBlockExpr of
        Err error ->
            Err ("Failed to compile case-of block expression: " ++ error)

        Ok expression ->
            case compileCaseBlockInline stack expression caseBlock.cases of
                Err err ->
                    Err err

                Ok inlineVariant ->
                    let
                        pseudoParamName =
                            {-
                               Adapt to current limitation in FirCompiler:
                               Since FirCompiler does not yet support shadowing, ensure we create a unique name here, by appending stack depth.
                            -}
                            "case-expr-" ++ String.fromInt stack.depth

                        innerExpr =
                            FirCompiler.ReferenceExpression pseudoParamName
                    in
                    case
                        compileCaseBlockInline stack innerExpr caseBlock.cases
                    of
                        Err err ->
                            Err err

                        Ok casesFunction ->
                            let
                                inlineVariantFuncApps =
                                    FirCompiler.listFunctionAppExpressions inlineVariant

                                casesFunctionFuncApps =
                                    FirCompiler.listFunctionAppExpressions casesFunction
                            in
                            if List.length casesFunctionFuncApps < List.length inlineVariantFuncApps then
                                Ok
                                    (FunctionApplicationExpression
                                        (FunctionExpression
                                            [ [ ( pseudoParamName, [] ) ] ]
                                            casesFunction
                                        )
                                        [ expression ]
                                    )

                            else
                                Ok inlineVariant


compileCaseBlockInline :
    CompilationStack
    -> Expression
    -> List Elm.Syntax.Expression.Case
    -> Result String Expression
compileCaseBlockInline stack caseBlockExpr caseBlockCases =
    case
        Common.resultListMapCombine
            (\elmCase ->
                compileElmSyntaxCaseBlockCase stack caseBlockExpr elmCase
            )
            caseBlockCases
    of
        Err error ->
            Err ("Failed to compile case in case-of block: " ++ error)

        Ok cases ->
            let
                conditionalFromCase deconstructedCase nextBlockExpression =
                    List.foldl
                        (\conditionExpression nextConditionExpression ->
                            ConditionalExpression
                                { condition = conditionExpression
                                , ifTrue = nextConditionExpression
                                , ifFalse = nextBlockExpression
                                }
                        )
                        deconstructedCase.thenExpression
                        deconstructedCase.conditionExpressions
            in
            Ok
                (List.foldr
                    conditionalFromCase
                    (PineFunctionApplicationExpression
                        -- Crash in case none of the branches match.
                        (Pine.ParseAndEvalExpression
                            { expression = Pine.LiteralExpression stringAsValue_errorNoMatchingBranch
                            , environment = Pine.EnvironmentExpression
                            }
                        )
                        caseBlockExpr
                    )
                    cases
                )


stringAsValue_errorNoMatchingBranch : Pine.Value
stringAsValue_errorNoMatchingBranch =
    Pine.valueFromString "Error in case-of block: No matching branch."


compileElmSyntaxCaseBlockCase :
    CompilationStack
    -> Expression
    -> Elm.Syntax.Expression.Case
    ->
        Result
            String
            { conditionExpressions : List Expression
            , thenExpression : Expression
            }
compileElmSyntaxCaseBlockCase stackBefore caseBlockValueExpression ( Elm.Syntax.Node.Node _ elmPattern, Elm.Syntax.Node.Node _ elmExpression ) =
    case compileElmSyntaxPattern stackBefore elmPattern of
        Err error ->
            Err error

        Ok deconstruction ->
            let
                deconstructionDeclarations =
                    List.foldl
                        (\( declName, deconsExpr ) ->
                            Dict.insert declName
                                (expressionForDeconstructions deconsExpr caseBlockValueExpression)
                        )
                        Dict.empty
                        deconstruction.declarations

                inlineableDeclarations =
                    Dict.foldl
                        (\declName declExpr aggregate ->
                            Dict.insert declName
                                (applicableDeclarationFromConstructorExpression declExpr)
                                aggregate
                        )
                        stackBefore.inlineableDeclarations
                        deconstructionDeclarations

                stack =
                    { stackBefore
                        | inlineableDeclarations = inlineableDeclarations
                    }
            in
            case compileElmSyntaxExpression stack elmExpression of
                Err err ->
                    Err err

                Ok expression ->
                    Ok
                        { conditionExpressions =
                            deconstruction.conditionExpressions caseBlockValueExpression
                        , thenExpression =
                            if deconstruction.declarations == [] then
                                expression

                            else
                                DeclarationBlockExpression
                                    deconstructionDeclarations
                                    expression
                        }


compileElmSyntaxPattern :
    CompilationStack
    -> Elm.Syntax.Pattern.Pattern
    ->
        Result
            String
            { conditionExpressions : Expression -> List Expression
            , declarations : List ( String, List Deconstruction )
            }
compileElmSyntaxPattern compilation elmPattern =
    let
        continueWithOnlyEqualsCondition :
            Expression
            -> Result error { conditionExpressions : Expression -> List Expression, declarations : List a }
        continueWithOnlyEqualsCondition valueToCompare =
            Ok
                { conditionExpressions =
                    \deconstructedExpression ->
                        [ equalCondition [ deconstructedExpression, valueToCompare ] ]
                , declarations = []
                }

        conditionsAndDeclarationsFromItemPattern :
            Int
            -> Elm.Syntax.Pattern.Pattern
            -> Result String { conditions : Expression -> List Expression, declarations : List ( String, List Deconstruction ) }
        conditionsAndDeclarationsFromItemPattern itemIndex itemPattern =
            case compileElmSyntaxPattern compilation itemPattern of
                Err err ->
                    Err err

                Ok listElementResult ->
                    Ok
                        { conditions =
                            \mapped ->
                                listElementResult.conditionExpressions
                                    (listItemFromIndexExpression itemIndex mapped)
                        , declarations =
                            List.map
                                (Tuple.mapSecond ((::) (ListItemDeconstruction itemIndex)))
                                listElementResult.declarations
                        }

        continueWithListOrTupleItems :
            List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
            -> Result String { conditionExpressions : Expression -> List Expression, declarations : List ( String, List Deconstruction ) }
        continueWithListOrTupleItems listItems =
            if listItems == [] then
                continueWithOnlyEqualsCondition (ListExpression [])
                {-
                   TODO: Analogous to the case of an empty list:
                   Optimize other cases that constrain to a single value by emitting an equality check.
                -}

            else
                case
                    Common.resultListIndexedMapCombine
                        (\argIndex (Elm.Syntax.Node.Node _ itemPattern) ->
                            conditionsAndDeclarationsFromItemPattern argIndex itemPattern
                        )
                        listItems
                of
                    Err err ->
                        Err err

                    Ok itemsResults ->
                        let
                            expectedLength =
                                List.length listItems

                            matchesLengthCondition : Expression -> Expression
                            matchesLengthCondition =
                                \deconstructedExpression ->
                                    let
                                        genericLengthCheckExpr () =
                                            equalCondition
                                                [ LiteralExpression (Pine.valueFromInt expectedLength)
                                                , countListElementsExpression deconstructedExpression
                                                ]
                                    in
                                    case deconstructedExpression of
                                        ListExpression deconstructedList ->
                                            LiteralExpression
                                                (if List.length deconstructedList == expectedLength then
                                                    Pine.trueValue

                                                 else
                                                    Pine.falseValue
                                                )

                                        _ ->
                                            genericLengthCheckExpr ()

                            conditionExpressions : Expression -> List Expression
                            conditionExpressions =
                                \deconstructedExpression ->
                                    matchesLengthCondition deconstructedExpression
                                        :: List.concatMap (.conditions >> (|>) deconstructedExpression) itemsResults
                        in
                        Ok
                            { conditionExpressions = conditionExpressions
                            , declarations = itemsResults |> List.concatMap .declarations
                            }
    in
    case elmPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok
                { conditionExpressions = always []
                , declarations = []
                }

        Elm.Syntax.Pattern.ListPattern listElements ->
            continueWithListOrTupleItems listElements

        Elm.Syntax.Pattern.TuplePattern tupleElements ->
            continueWithListOrTupleItems tupleElements

        Elm.Syntax.Pattern.UnConsPattern (Elm.Syntax.Node.Node _ unconsLeft) (Elm.Syntax.Node.Node _ unconsRight) ->
            case compileElmSyntaxPattern compilation unconsLeft of
                Err err ->
                    Err err

                Ok leftSide ->
                    case compileElmSyntaxPattern compilation unconsRight of
                        Err err ->
                            Err err

                        Ok rightSide ->
                            let
                                conditionExpressions =
                                    \deconstructedExpression ->
                                        List.concat
                                            [ [ KernelApplicationExpression
                                                    { functionName = "negate"
                                                    , argument =
                                                        equalCondition
                                                            [ deconstructedExpression
                                                            , listSkipExpression 1 deconstructedExpression
                                                            ]
                                                    }
                                              ]
                                            , leftSide.conditionExpressions
                                                (listItemFromIndexExpression 0 deconstructedExpression)
                                            , rightSide.conditionExpressions
                                                (listSkipExpression 1 deconstructedExpression)
                                            ]

                                declarations =
                                    List.concat
                                        [ leftSide.declarations
                                            |> List.map (Tuple.mapSecond ((::) (ListItemDeconstruction 0)))
                                        , rightSide.declarations
                                            |> List.map (Tuple.mapSecond ((::) (SkipItemsDeconstruction 1)))
                                        ]
                            in
                            Ok
                                { conditionExpressions = conditionExpressions
                                , declarations = declarations
                                }

        Elm.Syntax.Pattern.NamedPattern qualifiedName choiceTypeArgumentPatterns ->
            case
                Common.resultListIndexedMapCombine
                    (\argIndex (Elm.Syntax.Node.Node _ argPattern) ->
                        case conditionsAndDeclarationsFromItemPattern argIndex argPattern of
                            Err err ->
                                Err
                                    ("Failed for named pattern argument "
                                        ++ String.fromInt argIndex
                                        ++ ": "
                                        ++ err
                                    )

                            Ok ok ->
                                Ok ok
                    )
                    choiceTypeArgumentPatterns
            of
                Err err ->
                    Err err

                Ok itemsResults ->
                    let
                        conditionExpressions : Expression -> List Expression
                        conditionExpressions =
                            \deconstructedExpression ->
                                let
                                    typeSourceModule =
                                        case qualifiedName.moduleName of
                                            [] ->
                                                Nothing

                                            _ ->
                                                case Dict.get qualifiedName.moduleName compilation.moduleAliases of
                                                    Just fromAlias ->
                                                        Just fromAlias

                                                    Nothing ->
                                                        Just qualifiedName.moduleName

                                    typeInfoMaybe : Maybe ( String, ElmModuleChoiceType )
                                    typeInfoMaybe =
                                        case typeSourceModule of
                                            Nothing ->
                                                {-
                                                   TODO: Expand lookup of type to also support cases of import all (exposing (..))
                                                -}
                                                case Dict.get qualifiedName.name compilation.localTypeDeclarations of
                                                    Nothing ->
                                                        Nothing

                                                    Just typeDeclaration ->
                                                        case typeDeclaration of
                                                            ElmModuleChoiceTypeDeclaration choiceTypeDeclaration ->
                                                                Just ( qualifiedName.name, choiceTypeDeclaration )

                                                            _ ->
                                                                Nothing

                                            Just sourceModuleName ->
                                                case Dict.get sourceModuleName compilation.availableModules of
                                                    Nothing ->
                                                        Nothing

                                                    Just moduleInCompilation ->
                                                        Common.listMapFind
                                                            (\( typeName, typeDeclaration ) ->
                                                                case typeDeclaration of
                                                                    ElmModuleChoiceTypeDeclaration choiceTypeDeclaration ->
                                                                        if Dict.member qualifiedName.name choiceTypeDeclaration.tags then
                                                                            Just ( typeName, choiceTypeDeclaration )

                                                                        else
                                                                            Nothing

                                                                    _ ->
                                                                        Nothing
                                                            )
                                                            (Dict.toList moduleInCompilation.typeDeclarations)

                                    tagIsOnlyPossible : Bool
                                    tagIsOnlyPossible =
                                        case typeInfoMaybe of
                                            Nothing ->
                                                False

                                            Just ( _, choiceType ) ->
                                                Dict.size choiceType.tags == 1

                                    matchingTagConditions =
                                        if tagIsOnlyPossible then
                                            []

                                        else
                                            [ case Dict.get qualifiedName.name elmDeclarationsOverridesExpressions of
                                                Just tagNameExpressionFromOverrides ->
                                                    equalCondition
                                                        [ tagNameExpressionFromOverrides
                                                        , deconstructedExpression
                                                        ]

                                                Nothing ->
                                                    equalCondition
                                                        [ LiteralExpression (Pine.valueFromString qualifiedName.name)
                                                        , pineKernel_ListHead deconstructedExpression
                                                        ]
                                            ]

                                    argumentsConditions =
                                        itemsResults
                                            |> List.concatMap
                                                (.conditions
                                                    >> (|>) (listItemFromIndexExpression 1 deconstructedExpression)
                                                )
                                in
                                List.concat [ matchingTagConditions, argumentsConditions ]

                        declarations =
                            itemsResults
                                |> List.concatMap .declarations
                                |> List.map (Tuple.mapSecond ((::) (ListItemDeconstruction 1)))
                    in
                    Ok
                        { conditionExpressions = conditionExpressions
                        , declarations = declarations
                        }

        Elm.Syntax.Pattern.CharPattern char ->
            continueWithOnlyEqualsCondition (LiteralExpression (Pine.valueFromChar char))

        Elm.Syntax.Pattern.IntPattern int ->
            continueWithOnlyEqualsCondition (LiteralExpression (Pine.valueFromInt int))

        Elm.Syntax.Pattern.StringPattern string ->
            continueWithOnlyEqualsCondition (LiteralExpression (valueFromString string))

        Elm.Syntax.Pattern.VarPattern name ->
            Ok
                { conditionExpressions = always []
                , declarations =
                    [ ( name
                      , []
                      )
                    ]
                }

        Elm.Syntax.Pattern.RecordPattern fieldsElements ->
            Ok
                { conditionExpressions = always []
                , declarations =
                    fieldsElements
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ fieldName) ->
                                ( fieldName
                                , [ Pine.ParseAndEvalExpression
                                        { expression = Pine.LiteralExpression pineFunctionForRecordAccessAsValue
                                        , environment =
                                            Pine.ListExpression
                                                [ Pine.environmentExpr
                                                , Pine.LiteralExpression (Pine.valueFromString fieldName)
                                                ]
                                        }
                                        |> PineFunctionApplicationDeconstruction
                                  ]
                                )
                            )
                }

        Elm.Syntax.Pattern.AsPattern (Elm.Syntax.Node.Node _ aliasedPattern) (Elm.Syntax.Node.Node _ alias) ->
            compileElmSyntaxPattern compilation aliasedPattern
                |> Result.map
                    (\aliasedResult ->
                        { aliasedResult
                            | declarations = ( alias, [] ) :: aliasedResult.declarations
                        }
                    )

        Elm.Syntax.Pattern.ParenthesizedPattern (Elm.Syntax.Node.Node _ parenthesized) ->
            compileElmSyntaxPattern compilation parenthesized

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                { conditionExpressions = always []
                , declarations = []
                }

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "Unsupported type of pattern: FloatPattern"

        Elm.Syntax.Pattern.HexPattern _ ->
            Err "Unsupported type of pattern: HexPattern"


{-| Uses simple type inference to search for optimized compilation of operator application.
For example, in a sequence of ++ operators: If any element is a string literal, within the rules
of Elm, all other elements also must be of type String.
This constraint of types allows for the emission of a specialized representation,
reducing compiled code size and runtime overhead.
-}
searchCompileElmSyntaxOperatorOptimized :
    CompilationStack
    -> String
    -> Elm.Syntax.Expression.Expression
    -> Elm.Syntax.Expression.Expression
    -> Maybe (Result String Expression)
searchCompileElmSyntaxOperatorOptimized stack operator leftExpr rightExpr =
    case operator of
        "==" ->
            {-
               For the general case, '==' will be compiled to 'Basics.eq', which is polymorphic and has a more complex
               implementation to handle 'Set' and 'Dict' instances.
               If we can prove at compile time that the operand type cannot contain any 'Set' or 'Dict',
               we can emit a direct usage of the 'equal' kernel function.
            -}
            let
                exprCannotContainSetOrDict : Elm.Syntax.Expression.Expression -> Bool
                exprCannotContainSetOrDict expr =
                    case expr of
                        Elm.Syntax.Expression.Literal _ ->
                            True

                        Elm.Syntax.Expression.CharLiteral _ ->
                            True

                        Elm.Syntax.Expression.Integer _ ->
                            True

                        Elm.Syntax.Expression.Hex _ ->
                            True

                        Elm.Syntax.Expression.Floatable _ ->
                            True

                        Elm.Syntax.Expression.ListExpr listExpr ->
                            List.all (\(Elm.Syntax.Node.Node _ listItem) -> exprCannotContainSetOrDict listItem) listExpr

                        Elm.Syntax.Expression.TupledExpression tupleExpr ->
                            List.all (\(Elm.Syntax.Node.Node _ listItem) -> exprCannotContainSetOrDict listItem) tupleExpr

                        Elm.Syntax.Expression.FunctionOrValue _ localName ->
                            {-
                               Cover choice type tags without arguments, like 'Nothing' or 'LT'.
                            -}
                            stringStartsWithUpper localName

                        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ parenthesized) ->
                            exprCannotContainSetOrDict parenthesized

                        _ ->
                            False

                operandCannotContainSetOrDict =
                    exprCannotContainSetOrDict leftExpr || exprCannotContainSetOrDict rightExpr
            in
            if not operandCannotContainSetOrDict then
                Nothing

            else
                case compileElmSyntaxExpression stack leftExpr of
                    Err err ->
                        Just (Err err)

                    Ok leftExprCompiled ->
                        case compileElmSyntaxExpression stack rightExpr of
                            Err err ->
                                Just (Err err)

                            Ok rightExprCompiled ->
                                Just
                                    (Ok
                                        (KernelApplicationExpression
                                            { functionName = "equal"
                                            , argument = ListExpression [ leftExprCompiled, rightExprCompiled ]
                                            }
                                        )
                                    )

        "++" ->
            let
                items =
                    List.concat
                        [ flattenOperatorAppSequencePlusPlus leftExpr
                        , flattenOperatorAppSequencePlusPlus rightExpr
                        ]

                anyItemIsString =
                    List.any
                        (\item ->
                            case item of
                                Elm.Syntax.Expression.Literal _ ->
                                    True

                                _ ->
                                    False
                        )
                        items
            in
            if not anyItemIsString then
                Nothing

            else
                case Common.resultListMapCombine (compileElmSyntaxExpression stack) items of
                    Err err ->
                        Just (Err err)

                    Ok expressions ->
                        let
                            {- Depend on the specific representation of Elm strings
                               to retrieve the lists of characters before concatenation and
                               compose the resulting 'String' after concatenation.
                            -}
                            stringsExpressions =
                                List.map
                                    (\stringExpr ->
                                        FirCompiler.listItemFromIndexExpression 0
                                            (FirCompiler.listItemFromIndexExpression 1 stringExpr)
                                    )
                                    expressions

                            concatExpr =
                                KernelApplicationExpression
                                    { functionName = "concat"
                                    , argument = ListExpression stringsExpressions
                                    }
                        in
                        Just
                            (Ok
                                (ListExpression
                                    [ LiteralExpression elmStringTypeTagNameAsValue
                                    , ListExpression [ concatExpr ]
                                    ]
                                )
                            )

        _ ->
            Nothing


flattenOperatorAppSequencePlusPlus :
    Elm.Syntax.Expression.Expression
    -> List Elm.Syntax.Expression.Expression
flattenOperatorAppSequencePlusPlus expr =
    case expr of
        Elm.Syntax.Expression.OperatorApplication operator _ (Elm.Syntax.Node.Node _ leftExpr) (Elm.Syntax.Node.Node _ rightExpr) ->
            if operator == "++" then
                List.concat
                    [ flattenOperatorAppSequencePlusPlus leftExpr
                    , flattenOperatorAppSequencePlusPlus rightExpr
                    ]

            else
                [ expr ]

        _ ->
            [ expr ]


mapExpressionForOperatorPrecedence : Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
mapExpressionForOperatorPrecedence originalExpression =
    case originalExpression of
        Elm.Syntax.Expression.OperatorApplication operator direction (Elm.Syntax.Node.Node leftRange leftExpr) (Elm.Syntax.Node.Node rightRange rightExpr) ->
            let
                operatorPriority =
                    case Dict.get operator operatorPrecendencePriority of
                        Nothing ->
                            0

                        Just priority ->
                            priority

                mappedLeftExpr =
                    mapExpressionForOperatorPrecedence leftExpr

                mappedRightExpr =
                    mapExpressionForOperatorPrecedence rightExpr

                orderedLeft =
                    case mappedLeftExpr of
                        Elm.Syntax.Expression.OperatorApplication leftOperator _ leftLeftExpr leftRightExpr ->
                            let
                                operatorLeftPriority =
                                    operatorPrecendencePriority |> Dict.get leftOperator |> Maybe.withDefault 0

                                areStillOrderedBySyntaxRange =
                                    compareLocations
                                        leftRange.start
                                        (Elm.Syntax.Node.range leftLeftExpr).start
                                        == LT
                            in
                            if
                                (operatorLeftPriority < operatorPriority)
                                    || ((operatorLeftPriority == operatorPriority) && areStillOrderedBySyntaxRange)
                            then
                                mapExpressionForOperatorPrecedence
                                    (Elm.Syntax.Expression.OperatorApplication leftOperator
                                        direction
                                        leftLeftExpr
                                        (Elm.Syntax.Node.Node
                                            (Elm.Syntax.Range.combine [ Elm.Syntax.Node.range leftRightExpr, rightRange ])
                                            (Elm.Syntax.Expression.OperatorApplication operator direction leftRightExpr (Elm.Syntax.Node.Node rightRange rightExpr))
                                        )
                                    )

                            else
                                originalExpression

                        _ ->
                            originalExpression
            in
            if mappedLeftExpr /= leftExpr || mappedRightExpr /= rightExpr then
                mapExpressionForOperatorPrecedence
                    (Elm.Syntax.Expression.OperatorApplication
                        operator
                        direction
                        (Elm.Syntax.Node.Node leftRange mappedLeftExpr)
                        (Elm.Syntax.Node.Node rightRange mappedRightExpr)
                    )

            else
                case mappedRightExpr of
                    Elm.Syntax.Expression.OperatorApplication rightOperator _ (Elm.Syntax.Node.Node rightLeftRange rightLeftExpr) rightRightExpr ->
                        let
                            operatorRightPriority =
                                operatorPrecendencePriority |> Dict.get rightOperator |> Maybe.withDefault 0

                            areStillOrderedBySyntaxRange =
                                compareLocations leftRange.start rightLeftRange.start
                                    == LT
                        in
                        if
                            (operatorRightPriority < operatorPriority)
                                || ((operatorRightPriority == operatorPriority) && areStillOrderedBySyntaxRange)
                        then
                            mapExpressionForOperatorPrecedence
                                (Elm.Syntax.Expression.OperatorApplication rightOperator
                                    direction
                                    (Elm.Syntax.Node.Node
                                        (Elm.Syntax.Range.combine
                                            [ leftRange
                                            , rightLeftRange
                                            ]
                                        )
                                        (Elm.Syntax.Expression.OperatorApplication
                                            operator
                                            direction
                                            (Elm.Syntax.Node.Node leftRange leftExpr)
                                            (Elm.Syntax.Node.Node rightLeftRange rightLeftExpr)
                                        )
                                    )
                                    rightRightExpr
                                )

                        else
                            orderedLeft

                    _ ->
                        orderedLeft

        _ ->
            originalExpression


compareLocations : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Order
compareLocations left right =
    if left.row < right.row then
        LT

    else if right.row < left.row then
        GT

    else
        compare left.column right.column


expressionForDeconstructions : List Deconstruction -> Expression -> Expression
expressionForDeconstructions deconsList expr =
    List.foldl
        expressionForDeconstruction
        expr
        deconsList


expressionForDeconstruction : Deconstruction -> Expression -> Expression
expressionForDeconstruction deconstruction =
    case deconstruction of
        ListItemDeconstruction index ->
            listItemFromIndexExpression index

        SkipItemsDeconstruction count ->
            listSkipExpression count

        PineFunctionApplicationDeconstruction pineFunctionValue ->
            PineFunctionApplicationExpression pineFunctionValue


pineFunctionForRecordUpdateAsValue : Pine.Value
pineFunctionForRecordUpdateAsValue =
    Pine.encodeExpressionAsValue pineFunctionForRecordUpdate


pineFunctionForRecordUpdate : Pine.Expression
pineFunctionForRecordUpdate =
    let
        recordExpression =
            listItemFromIndexExpression_Pine 0 Pine.environmentExpr

        recordFieldsExpression =
            pineKernel_ListHead_Pine (listItemFromIndexExpression_Pine 1 recordExpression)

        fieldsUpdatesExpression =
            listItemFromIndexExpression_Pine 1 Pine.environmentExpr

        recursiveFunction : Pine.Value
        recursiveFunction =
            Pine.encodeExpressionAsValue recursiveFunctionToUpdateFieldsInRecord
    in
    Pine.ConditionalExpression
        { condition =
            equalCondition_Pine
                [ Pine.LiteralExpression elmRecordTypeTagNameAsValue
                , pineKernel_ListHead_Pine recordExpression
                ]
        , ifTrue =
            Pine.ListExpression
                [ Pine.LiteralExpression elmRecordTypeTagNameAsValue
                , Pine.ListExpression
                    [ Pine.ParseAndEvalExpression
                        { expression = Pine.LiteralExpression recursiveFunction
                        , environment =
                            Pine.ListExpression
                                [ Pine.LiteralExpression recursiveFunction
                                , fieldsUpdatesExpression
                                , Pine.ListExpression []
                                , recordFieldsExpression
                                ]
                        }
                    ]
                ]
        , ifFalse =
            Pine.ParseAndEvalExpression
                { expression =
                    Pine.LiteralExpression
                        (Pine.valueFromString "invalid record update - not a record")
                , environment = recordExpression
                }
        }


{-| Recursively scans through the record fields and replaces every field contained in the argument list.
The argument list contains pairs of field names and new values.

Takes the following arguments:

1.  The function itself, so that we don't have to depend on recursion in the environment.
2.  A list of pairs of field names and new values.
3.  The list of fields that have been processed so far.
4.  The list of fields that are yet to be processed.

-}
recursiveFunctionToUpdateFieldsInRecord : Pine.Expression
recursiveFunctionToUpdateFieldsInRecord =
    let
        functionReferenceLocalExpression : Pine.Expression
        functionReferenceLocalExpression =
            listItemFromIndexExpression_Pine 0 Pine.environmentExpr

        fieldPairsLocalExpression : Pine.Expression
        fieldPairsLocalExpression =
            listItemFromIndexExpression_Pine 1 Pine.environmentExpr

        processedFieldsLocalExpression : Pine.Expression
        processedFieldsLocalExpression =
            listItemFromIndexExpression_Pine 2 Pine.environmentExpr

        remainingFieldsLocalExpression : Pine.Expression
        remainingFieldsLocalExpression =
            listItemFromIndexExpression_Pine 3 Pine.environmentExpr

        remainingFieldsNextLocalExpression : Pine.Expression
        remainingFieldsNextLocalExpression =
            listItemFromIndexExpression_Pine 0 remainingFieldsLocalExpression

        firstFieldPairLocalExpression : Pine.Expression
        firstFieldPairLocalExpression =
            listItemFromIndexExpression_Pine 0 fieldPairsLocalExpression

        firstFieldNameLocalExpression : Pine.Expression
        firstFieldNameLocalExpression =
            listItemFromIndexExpression_Pine 0 firstFieldPairLocalExpression
    in
    Pine.ConditionalExpression
        { condition =
            equalCondition_Pine
                [ Pine.ListExpression []
                , fieldPairsLocalExpression
                ]
        , ifTrue =
            Pine.KernelApplicationExpression
                { functionName = "concat"
                , argument =
                    Pine.ListExpression
                        [ processedFieldsLocalExpression
                        , remainingFieldsLocalExpression
                        ]
                }
        , ifFalse =
            Pine.ConditionalExpression
                { condition =
                    equalCondition_Pine
                        [ Pine.ListExpression []
                        , remainingFieldsLocalExpression
                        ]
                , ifTrue =
                    Pine.ParseAndEvalExpression
                        { expression =
                            Pine.LiteralExpression
                                (Pine.valueFromString "invalid record update - field name not found")
                        , environment = firstFieldNameLocalExpression
                        }
                , ifFalse =
                    Pine.ConditionalExpression
                        { condition =
                            equalCondition_Pine
                                [ listItemFromIndexExpression_Pine 0 remainingFieldsNextLocalExpression
                                , firstFieldNameLocalExpression
                                ]
                        , ifTrue =
                            Pine.ParseAndEvalExpression
                                { expression = functionReferenceLocalExpression
                                , environment =
                                    Pine.ListExpression
                                        [ functionReferenceLocalExpression
                                        , listSkipExpression_Pine 1 fieldPairsLocalExpression
                                        , Pine.KernelApplicationExpression
                                            { functionName = "concat"
                                            , argument =
                                                Pine.ListExpression
                                                    [ processedFieldsLocalExpression
                                                    , Pine.ListExpression
                                                        [ firstFieldPairLocalExpression ]
                                                    ]
                                            }
                                        , listSkipExpression_Pine 1 remainingFieldsLocalExpression
                                        ]
                                }
                        , ifFalse =
                            Pine.ParseAndEvalExpression
                                { expression = functionReferenceLocalExpression
                                , environment =
                                    Pine.ListExpression
                                        [ functionReferenceLocalExpression
                                        , fieldPairsLocalExpression
                                        , Pine.KernelApplicationExpression
                                            { functionName = "concat"
                                            , argument =
                                                Pine.ListExpression
                                                    [ processedFieldsLocalExpression
                                                    , Pine.ListExpression
                                                        [ remainingFieldsNextLocalExpression ]
                                                    ]
                                            }
                                        , listSkipExpression_Pine 1 remainingFieldsLocalExpression
                                        ]
                                }
                        }
                }
        }


pineFunctionForRecordAccessAsValue : Pine.Value
pineFunctionForRecordAccessAsValue =
    Pine.encodeExpressionAsValue pineFunctionForRecordAccess


pineFunctionForRecordAccess : Pine.Expression
pineFunctionForRecordAccess =
    let
        recordExpression =
            listItemFromIndexExpression_Pine 0 Pine.environmentExpr

        fieldNameLocalExpression =
            listItemFromIndexExpression_Pine 1 Pine.environmentExpr

        recordFieldsExpression =
            pineKernel_ListHead_Pine (listItemFromIndexExpression_Pine 1 recordExpression)
    in
    Pine.ConditionalExpression
        { condition =
            equalCondition_Pine
                [ Pine.LiteralExpression elmRecordTypeTagNameAsValue
                , pineKernel_ListHead_Pine recordExpression
                ]
        , ifTrue =
            Pine.ParseAndEvalExpression
                { expression = Pine.LiteralExpression recursiveFunctionToLookupFieldInRecordAsValue
                , environment =
                    Pine.ListExpression
                        [ Pine.LiteralExpression recursiveFunctionToLookupFieldInRecordAsValue
                        , fieldNameLocalExpression
                        , recordFieldsExpression
                        ]
                }
        , ifFalse =
            Pine.ParseAndEvalExpression
                { expression =
                    Pine.LiteralExpression
                        (Pine.valueFromString "invalid record access - not a record")
                , environment = fieldNameLocalExpression
                }
        }


recursiveFunctionToLookupFieldInRecordAsValue : Pine.Value
recursiveFunctionToLookupFieldInRecordAsValue =
    Pine.encodeExpressionAsValue recursiveFunctionToLookupFieldInRecord


recursiveFunctionToLookupFieldInRecord : Pine.Expression
recursiveFunctionToLookupFieldInRecord =
    let
        selfFunctionLocalExpression =
            listItemFromIndexExpression_Pine 0 Pine.environmentExpr

        fieldNameLocalExpression =
            listItemFromIndexExpression_Pine 1 Pine.environmentExpr

        remainingFieldsLocalExpression =
            listItemFromIndexExpression_Pine 2 Pine.environmentExpr

        continueWithRemainingExpression =
            Pine.ParseAndEvalExpression
                { expression = selfFunctionLocalExpression
                , environment =
                    Pine.ListExpression
                        [ selfFunctionLocalExpression
                        , fieldNameLocalExpression
                        , listSkipExpression_Pine 1 remainingFieldsLocalExpression
                        ]
                }
    in
    Pine.ConditionalExpression
        { condition =
            equalCondition_Pine
                [ Pine.ListExpression []
                , remainingFieldsLocalExpression
                ]
        , ifTrue =
            Pine.ParseAndEvalExpression
                { expression =
                    Pine.LiteralExpression
                        (Pine.valueFromString "invalid record access - field name not found")
                , environment = fieldNameLocalExpression
                }
        , ifFalse =
            Pine.ConditionalExpression
                { condition =
                    equalCondition_Pine
                        [ remainingFieldsLocalExpression
                            |> listItemFromIndexExpression_Pine 0
                            |> listItemFromIndexExpression_Pine 0
                        , fieldNameLocalExpression
                        ]
                , ifTrue =
                    remainingFieldsLocalExpression
                        |> listItemFromIndexExpression_Pine 0
                        |> listItemFromIndexExpression_Pine 1
                , ifFalse = continueWithRemainingExpression
                }
        }


compileElmFunctionOrValueLookup : ( List String, String ) -> CompilationStack -> Result String Expression
compileElmFunctionOrValueLookup ( moduleName, localName ) compilation =
    if moduleName == [] then
        case Dict.get localName compilation.inlineableDeclarations of
            Nothing ->
                compileElmFunctionOrValueLookupWithoutLocalResolution ( moduleName, localName ) compilation

            Just applicableDeclaration ->
                Ok (applicableDeclaration [])

    else
        getDeclarationValueFromCompilation ( moduleName, localName ) compilation
            |> Result.map (compileLookupForInlineableDeclaration ( moduleName, localName ))


compileElmFunctionOrValueLookupWithoutLocalResolution :
    ( List String, String )
    -> CompilationStack
    -> Result String Expression
compileElmFunctionOrValueLookupWithoutLocalResolution ( moduleName, name ) compilation =
    let
        fusedName =
            String.join "." (List.concat [ moduleName, [ name ] ])
    in
    case Dict.get name elmDeclarationsOverridesExpressions of
        Just declarationOverride ->
            Ok declarationOverride

        Nothing ->
            case Dict.get name compilation.elmValuesToExposeToGlobal of
                Nothing ->
                    Ok (ReferenceExpression fusedName)

                Just sourceModuleName ->
                    getDeclarationValueFromCompilation ( sourceModuleName, name ) compilation
                        |> Result.map (compileLookupForInlineableDeclaration ( moduleName, name ))


getDeclarationValueFromCompilation : ( List String, String ) -> CompilationStack -> Result String Expression
getDeclarationValueFromCompilation ( localModuleName, nameInModule ) compilation =
    let
        canonicalModuleName =
            Dict.get localModuleName compilation.moduleAliases
                |> Maybe.withDefault localModuleName

        flatName =
            String.join "." (List.concat [ canonicalModuleName, [ nameInModule ] ])
    in
    case compilation.availableModules |> Dict.get canonicalModuleName of
        Nothing ->
            Err
                ("Did not find module '"
                    ++ String.join "." canonicalModuleName
                    ++ "'. There are "
                    ++ String.fromInt (Dict.size compilation.availableModules)
                    ++ " declarations in this scope: "
                    ++ String.join ", " (List.map (String.join ".") (Dict.keys compilation.availableModules))
                )

        Just moduleValue ->
            case Dict.get nameInModule moduleValue.functionDeclarations of
                Nothing ->
                    case Dict.get flatName compilation.inlineableDeclarations of
                        Just applicableDeclaration ->
                            Ok (applicableDeclaration [])

                        Nothing ->
                            let
                                declsReport =
                                    if stringStartsWithUpper nameInModule then
                                        let
                                            allTypesNames =
                                                Dict.foldl
                                                    (\typeName value aggregate ->
                                                        case value of
                                                            ElmModuleChoiceTypeDeclaration choiceType ->
                                                                List.concat [ [ typeName ], Dict.keys choiceType.tags, aggregate ]

                                                            ElmModuleRecordTypeDeclaration _ ->
                                                                typeName :: aggregate
                                                    )
                                                    []
                                                    moduleValue.typeDeclarations
                                        in
                                        "There are "
                                            ++ String.fromInt (List.length allTypesNames)
                                            ++ " type declarations available in that module: "
                                            ++ String.join ", " allTypesNames

                                    else
                                        "There are "
                                            ++ String.fromInt (Dict.size moduleValue.functionDeclarations)
                                            ++ " function declarations available in that module: "
                                            ++ String.join ", " (Dict.keys moduleValue.functionDeclarations)
                            in
                            Err
                                ("Did not find '"
                                    ++ nameInModule
                                    ++ "' in module '"
                                    ++ String.join "." canonicalModuleName
                                    ++ "'. "
                                    ++ declsReport
                                )

                Just declarationValue ->
                    Ok (LiteralExpression declarationValue)


compileLookupForInlineableDeclaration : ( List String, String ) -> Expression -> Expression
compileLookupForInlineableDeclaration ( moduleName, name ) expression =
    let
        fusedName =
            String.join "." (List.concat [ moduleName, [ name ] ])
    in
    if shouldInlineDeclaration name expression then
        expression

    else
        ReferenceExpression fusedName


{-| Encodes an Elm module into a transportable form.
-}
emitModuleValue : ElmModuleInCompilation -> Pine.Value
emitModuleValue parsedModule =
    let
        typeDescriptions : List ( String, Pine.Value )
        typeDescriptions =
            parsedModule.typeDeclarations
                |> Dict.toList
                |> List.map (Tuple.mapSecond emitTypeDeclarationValue)

        emittedFunctions =
            Dict.toList parsedModule.functionDeclarations
    in
    List.concat [ emittedFunctions, typeDescriptions ]
        |> List.map Pine.valueFromContextExpansionWithName
        |> Pine.ListValue


emitTypeDeclarationValue : ElmModuleTypeDeclaration -> Pine.Value
emitTypeDeclarationValue typeDeclaration =
    case typeDeclaration of
        ElmModuleChoiceTypeDeclaration choiceType ->
            emitChoiceTypeValue choiceType

        ElmModuleRecordTypeDeclaration fields ->
            emitRecordConstructorValue fields


emitChoiceTypeValue : ElmModuleChoiceType -> Pine.Value
emitChoiceTypeValue choiceType =
    Pine.valueFromContextExpansionWithName
        ( "ChoiceType"
        , choiceType.tags
            |> Dict.toList
            |> List.map
                (\( tagName, { argumentsCount } ) ->
                    Pine.ListValue
                        [ Pine.valueFromString tagName
                        , Pine.valueFromInt argumentsCount
                        ]
                )
            |> Pine.ListValue
        )


emitRecordConstructorValue : List String -> Pine.Value
emitRecordConstructorValue fields =
    Pine.valueFromContextExpansionWithName
        ( "RecordConstructor"
        , fields
            |> List.map Pine.valueFromString
            |> Pine.ListValue
        )


type alias EmittedRecursionDomain =
    { emittedDeclarations : List ( FirCompiler.EnvironmentFunctionEntry, ( Pine.Expression, Pine.Value ) )
    , exposedDeclarations : List ( String, Pine.Value )
    }


emitModuleFunctionDeclarations :
    EmitStack
    ->
        { exposedDeclarations : Dict.Dict String Expression
        , supportingDeclarations : Dict.Dict String Expression
        }
    -> Result String (List ( String, Pine.Value ))
emitModuleFunctionDeclarations stackBefore declarations =
    let
        exposedDeclarationsNames : Set.Set String
        exposedDeclarationsNames =
            Set.fromList (Dict.keys declarations.exposedDeclarations)

        allModuleDeclarations =
            Dict.union declarations.exposedDeclarations declarations.supportingDeclarations

        importedFunctionsNotShadowed : Dict.Dict String ( FirCompiler.EnvironmentFunctionEntry, Pine.Value )
        importedFunctionsNotShadowed =
            Dict.filter
                (\importedFunctionName _ ->
                    not (Dict.member importedFunctionName allModuleDeclarations)
                )
                stackBefore.importedFunctions

        declarationsDirectDependencies : Dict.Dict String (Set.Set String)
        declarationsDirectDependencies =
            Dict.foldl
                (\declName declExpr ->
                    Dict.insert declName (FirCompiler.listDirectDependenciesOfExpression declExpr)
                )
                Dict.empty
                allModuleDeclarations

        aggregateTransitiveDependencies : Set.Set String
        aggregateTransitiveDependencies =
            FirCompiler.getTransitiveDependencies
                declarationsDirectDependencies
                exposedDeclarationsNames

        declarationsTransitiveDependencies : Dict.Dict String (Set.Set String)
        declarationsTransitiveDependencies =
            Dict.foldl
                (\declarationName directDependencies aggregate ->
                    if Set.member declarationName aggregateTransitiveDependencies then
                        Dict.insert
                            declarationName
                            (FirCompiler.getTransitiveDependencies
                                declarationsDirectDependencies
                                directDependencies
                            )
                            aggregate

                    else
                        aggregate
                )
                Dict.empty
                declarationsDirectDependencies

        usedImportsAvailableEmittedFunctions : List ( FirCompiler.EnvironmentFunctionEntry, Pine.Value )
        usedImportsAvailableEmittedFunctions =
            Dict.values importedFunctionsNotShadowed

        recursionDomains : List (Set.Set String)
        recursionDomains =
            FirCompiler.recursionDomainsFromDeclarationDependencies
                declarationsTransitiveDependencies

        emitStack =
            { stackBefore
                | declarationsDependencies =
                    Dict.union
                        declarationsDirectDependencies
                        stackBefore.declarationsDependencies
            }

        emitRecursionDomainsRecursive :
            List EmittedRecursionDomain
            -> List (Set.Set String)
            -> Result String (List EmittedRecursionDomain)
        emitRecursionDomainsRecursive alreadyEmitted remainingRecursionDomains =
            case remainingRecursionDomains of
                [] ->
                    Ok alreadyEmitted

                currentRecursionDomain :: followingRecursionDomains ->
                    emitRecursionDomain currentRecursionDomain alreadyEmitted
                        |> Result.andThen
                            (\emittedDomain ->
                                emitRecursionDomainsRecursive
                                    (List.concat [ alreadyEmitted, [ emittedDomain ] ])
                                    followingRecursionDomains
                            )

        emitRecursionDomain :
            Set.Set String
            -> List EmittedRecursionDomain
            -> Result String EmittedRecursionDomain
        emitRecursionDomain currentRecursionDomain alreadyEmitted =
            let
                recursionDomainExposedNames : Set.Set String
                recursionDomainExposedNames =
                    Set.intersect currentRecursionDomain exposedDeclarationsNames

                recursionDomainDeclarations : Dict.Dict String Expression
                recursionDomainDeclarations =
                    Dict.filter
                        (\declName _ -> Set.member declName currentRecursionDomain)
                        allModuleDeclarations

                availableFunctionsValues : List ( FirCompiler.EnvironmentFunctionEntry, Pine.Value )
                availableFunctionsValues =
                    List.concatMap
                        (\emittedDomain ->
                            List.map (\( declName, ( _, emittedValue ) ) -> ( declName, emittedValue ))
                                emittedDomain.emittedDeclarations
                        )
                        alreadyEmitted

                availableEmittedFunctionsIncludingImports : List ( FirCompiler.EnvironmentFunctionEntry, Pine.Value )
                availableEmittedFunctionsIncludingImports =
                    List.concat
                        [ usedImportsAvailableEmittedFunctions, availableFunctionsValues ]

                recursionDomainDeclarationsToIncludeInBlock : Set.Set String
                recursionDomainDeclarationsToIncludeInBlock =
                    Set.foldl
                        (\declName aggregate ->
                            case Dict.get declName declarationsDirectDependencies of
                                Nothing ->
                                    aggregate

                                Just directDependencies ->
                                    Set.union directDependencies aggregate
                        )
                        Set.empty
                        currentRecursionDomain

                recursionDomainDeclarationsInBlock : Dict.Dict String Expression
                recursionDomainDeclarationsInBlock =
                    Dict.filter
                        (\declName _ -> Set.member declName recursionDomainDeclarationsToIncludeInBlock)
                        recursionDomainDeclarations

                importedFunctions : Dict.Dict String ( FirCompiler.EnvironmentFunctionEntry, Pine.Value )
                importedFunctions =
                    List.foldl
                        (\( importRecord, value ) aggregate ->
                            Dict.insert importRecord.functionName ( importRecord, value ) aggregate
                        )
                        Dict.empty
                        availableEmittedFunctionsIncludingImports
            in
            FirCompiler.emitDeclarationBlock
                { emitStack | importedFunctions = importedFunctions }
                recursionDomainDeclarationsInBlock
                { closureCaptures = []
                , additionalDeps = Dict.values recursionDomainDeclarations
                }
                |> Result.andThen
                    (\( blockEmitStack, blockDeclarationsEmitted ) ->
                        recursionDomainDeclarations
                            |> Dict.toList
                            |> Common.resultListMapCombine
                                (\( declarationName, declarationExpression ) ->
                                    let
                                        getFunctionInnerExpressionFromIndex : Int -> Pine.Expression
                                        getFunctionInnerExpressionFromIndex declarationIndex =
                                            let
                                                getEnvFunctionsExpression =
                                                    Pine.environmentExpr
                                                        |> listItemFromIndexExpression_Pine 0
                                            in
                                            Pine.LiteralExpression
                                                (Pine.encodeExpressionAsValue
                                                    (Pine.ParseAndEvalExpression
                                                        { expression =
                                                            FirCompiler.listItemFromIndexExpression_Pine
                                                                declarationIndex
                                                                getEnvFunctionsExpression
                                                        , environment = Pine.environmentExpr
                                                        }
                                                    )
                                                )

                                        retrieveOrBuildResult :
                                            Result
                                                String
                                                { getFunctionInnerExpression : Pine.Expression
                                                , parameterCount : Int
                                                , innerExpression : Pine.Expression
                                                , innerExpressionValue : Pine.Value
                                                }
                                        retrieveOrBuildResult =
                                            case
                                                Common.listFindWithIndex
                                                    (\functionEntry -> functionEntry.functionName == declarationName)
                                                    blockEmitStack.environmentFunctions
                                            of
                                                Just ( declarationIndex, declarationEntry ) ->
                                                    case
                                                        Common.listFind
                                                            (\( functionEntry, _ ) -> functionEntry.functionName == declarationName)
                                                            blockDeclarationsEmitted.newEnvFunctionsValues
                                                    of
                                                        Nothing ->
                                                            Err ("Compiler error: Missing entry: " ++ declarationName)

                                                        Just ( _, ( declEmittedExpr, declEmittedValue ) ) ->
                                                            Ok
                                                                { parameterCount = declarationEntry.parameterCount
                                                                , getFunctionInnerExpression = getFunctionInnerExpressionFromIndex declarationIndex
                                                                , innerExpression = declEmittedExpr
                                                                , innerExpressionValue = declEmittedValue
                                                                }

                                                Nothing ->
                                                    let
                                                        ( parsedDeclaration, emitDeclarationResult ) =
                                                            blockDeclarationsEmitted.parseAndEmitFunction declarationExpression
                                                    in
                                                    emitDeclarationResult
                                                        |> Result.map
                                                            (\declEmittedExpr ->
                                                                let
                                                                    innerExpressionValue =
                                                                        Pine.encodeExpressionAsValue declEmittedExpr
                                                                in
                                                                { parameterCount = List.length parsedDeclaration.parameters
                                                                , getFunctionInnerExpression = Pine.LiteralExpression innerExpressionValue
                                                                , innerExpression = declEmittedExpr
                                                                , innerExpressionValue = innerExpressionValue
                                                                }
                                                            )
                                    in
                                    case retrieveOrBuildResult of
                                        Err err ->
                                            Err err

                                        Ok declMatch ->
                                            evaluateAsIndependentExpression
                                                (if declMatch.parameterCount < 1 then
                                                    FirCompiler.emitWrapperForPartialApplicationZero
                                                        { getFunctionInnerExpression = declMatch.getFunctionInnerExpression
                                                        , getEnvFunctionsExpression = blockDeclarationsEmitted.envFunctionsExpression
                                                        }

                                                 else
                                                    FirCompiler.buildRecordOfPartiallyAppliedFunction
                                                        { getFunctionInnerExpression = declMatch.getFunctionInnerExpression
                                                        , parameterCount = declMatch.parameterCount
                                                        , getEnvFunctionsExpression = blockDeclarationsEmitted.envFunctionsExpression
                                                        , argumentsAlreadyCollected = []
                                                        }
                                                )
                                                |> Result.mapError ((++) ("Failed for declaration '" ++ declarationName ++ "': "))
                                                |> Result.map
                                                    (\wrappedForExpose ->
                                                        ( declarationName
                                                        , ( wrappedForExpose
                                                          , ( declMatch.parameterCount
                                                            , ( declMatch.innerExpression, declMatch.innerExpressionValue )
                                                            )
                                                          )
                                                        )
                                                    )
                                )
                            |> Result.mapError
                                (\err ->
                                    "Failed in recursion domain: "
                                        ++ String.join ", " (Set.toList currentRecursionDomain)
                                        ++ ": "
                                        ++ err
                                        ++ "\navailableFunctionsValues:\n"
                                        ++ String.join "\n"
                                            (reportEmittedDeclarationsForErrorMsg (List.map Tuple.first availableFunctionsValues))
                                )
                            |> Result.map
                                (\emittedForExposeOrReuse ->
                                    let
                                        expectedEnvironmentFunctions : List String
                                        expectedEnvironmentFunctions =
                                            List.map .functionName blockEmitStack.environmentFunctions

                                        emittedDeclarationsFromBlock : List ( FirCompiler.EnvironmentFunctionEntry, ( Pine.Expression, Pine.Value ) )
                                        emittedDeclarationsFromBlock =
                                            blockDeclarationsEmitted.newEnvFunctionsValues

                                        emittedDeclarationsFromBlockNames : Set.Set String
                                        emittedDeclarationsFromBlockNames =
                                            List.foldl (\( { functionName }, _ ) -> Set.insert functionName)
                                                Set.empty
                                                emittedDeclarationsFromBlock

                                        emittedDeclarationsFromExposed : List ( FirCompiler.EnvironmentFunctionEntry, ( Pine.Expression, Pine.Value ) )
                                        emittedDeclarationsFromExposed =
                                            emittedForExposeOrReuse
                                                |> List.map
                                                    (\( functionName, ( _, ( parameterCount, innerExpression ) ) ) ->
                                                        ( { functionName = functionName
                                                          , parameterCount = parameterCount
                                                          , expectedEnvironment =
                                                                FirCompiler.LocalEnvironment
                                                                    { expectedDecls = expectedEnvironmentFunctions }
                                                          }
                                                        , innerExpression
                                                        )
                                                    )

                                        emittedValueIsFunctionRecord : Pine.Value -> Bool
                                        emittedValueIsFunctionRecord value =
                                            case FirCompiler.parseFunctionRecordFromValueTagged value of
                                                Ok _ ->
                                                    True

                                                Err _ ->
                                                    False

                                        emittedDeclarations : List ( FirCompiler.EnvironmentFunctionEntry, ( Pine.Expression, Pine.Value ) )
                                        emittedDeclarations =
                                            List.concat
                                                [ emittedDeclarationsFromBlock
                                                , List.filter
                                                    (\( { functionName }, _ ) ->
                                                        not (Set.member functionName emittedDeclarationsFromBlockNames)
                                                    )
                                                    emittedDeclarationsFromExposed
                                                ]

                                        exposedDeclarations : List ( String, Pine.Value )
                                        exposedDeclarations =
                                            List.foldr
                                                (\( declName, ( wrappedForExpose, _ ) ) aggregate ->
                                                    if Set.member declName recursionDomainExposedNames then
                                                        ( declName, wrappedForExpose ) :: aggregate

                                                    else
                                                        aggregate
                                                )
                                                []
                                                emittedForExposeOrReuse
                                    in
                                    { emittedDeclarations = emittedDeclarations
                                    , exposedDeclarations = exposedDeclarations
                                    }
                                )
                    )
    in
    emitRecursionDomainsRecursive
        []
        recursionDomains
        |> Result.map (\domains -> List.concatMap .exposedDeclarations domains)


reportEmittedDeclarationsForErrorMsg :
    List FirCompiler.EnvironmentFunctionEntry
    -> List String
reportEmittedDeclarationsForErrorMsg emittedDeclarations =
    emittedDeclarations
        |> List.map
            (\funcEntry ->
                funcEntry.functionName
                    ++ " ("
                    ++ String.fromInt funcEntry.parameterCount
                    ++ ") env = "
                    ++ (case funcEntry.expectedEnvironment of
                            FirCompiler.LocalEnvironment { expectedDecls } ->
                                "local: "
                                    ++ String.fromInt (List.length expectedDecls)
                                    ++ " ("
                                    ++ String.join ", " expectedDecls
                                    ++ ")"

                            FirCompiler.ImportedEnvironment _ ->
                                "imported"

                            FirCompiler.IndependentEnvironment ->
                                "independent"
                       )
            )


compileElmChoiceTypeTagConstructor : { tagName : String, argumentsCount : Int } -> (List Expression -> Expression)
compileElmChoiceTypeTagConstructor { tagName, argumentsCount } =
    let
        tagNameAsValue =
            Pine.valueFromString tagName

        ( _, genericContructorValue ) =
            compileElmChoiceTypeTagConstructorValue { tagName = tagName, argumentsCount = argumentsCount }
    in
    \arguments ->
        if List.length arguments == argumentsCount then
            inlineElmSyntaxValueConstructor
                tagNameAsValue
                arguments

        else
            applicableDeclarationFromConstructorExpression
                (LiteralExpression genericContructorValue)
                arguments


applicableDeclarationFromConstructorExpression : Expression -> (List Expression -> Expression)
applicableDeclarationFromConstructorExpression genericContructorExpression =
    \arguments ->
        if arguments == [] then
            genericContructorExpression

        else
            FunctionApplicationExpression
                genericContructorExpression
                arguments


{-| Directly inlines an application of a choice type tag constructor for cases where number of applied
arguments matches the number of arguments of the constructor.
-}
inlineElmSyntaxValueConstructor : Pine.Value -> List Expression -> Expression
inlineElmSyntaxValueConstructor tagNameAsValue arguments =
    ListExpression
        [ LiteralExpression tagNameAsValue
        , ListExpression arguments
        ]


compileElmChoiceTypeTagConstructorValue : { tagName : String, argumentsCount : Int } -> ( String, Pine.Value )
compileElmChoiceTypeTagConstructorValue { tagName, argumentsCount } =
    ( tagName
    , case argumentsCount of
        0 ->
            Pine.ListValue
                [ Pine.valueFromString tagName
                , Pine.ListValue []
                ]

        1 ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString tagName)
                , Pine.ListExpression [ Pine.environmentExpr ]
                ]
                |> Pine.encodeExpressionAsValue

        2 ->
            Pine.ListExpression
                [ Pine.LiteralExpression Pine.stringAsValue_List
                , Pine.ListExpression
                    [ Pine.ListExpression
                        [ Pine.LiteralExpression Pine.stringAsValue_Literal
                        , Pine.LiteralExpression (Pine.valueFromString tagName)
                        ]
                    , Pine.ListExpression
                        [ Pine.LiteralExpression Pine.stringAsValue_List
                        , Pine.ListExpression
                            [ Pine.ListExpression
                                [ Pine.LiteralExpression Pine.stringAsValue_Literal
                                , Pine.environmentExpr
                                ]
                            , Pine.environmentExpr
                                |> Pine.encodeExpressionAsValue
                                |> Pine.LiteralExpression
                            ]
                        ]
                    ]
                ]
                |> Pine.encodeExpressionAsValue

        _ ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString tagName)
                , List.range 0 (argumentsCount - 1)
                    |> List.map
                        (\paramIndex ->
                            Pine.environmentExpr
                                |> listItemFromIndexExpression_Pine 1
                                |> listItemFromIndexExpression_Pine paramIndex
                        )
                    |> Pine.ListExpression
                ]
                |> emitWrapperForPartialApplication (Pine.ListExpression []) argumentsCount
                |> evaluateAsIndependentExpression
                |> Result.withDefault
                    (Pine.valueFromString "Failed to compile choice type tag constructor")
    )


compileElmRecordConstructor : List String -> (List Expression -> Expression)
compileElmRecordConstructor recordFieldNames =
    let
        recordFieldNamesStringAndValue : List ( String, Pine.Value )
        recordFieldNamesStringAndValue =
            List.map (\asString -> ( asString, Pine.valueFromString asString )) recordFieldNames
    in
    \arguments ->
        if List.length arguments == List.length recordFieldNamesStringAndValue then
            ListExpression
                [ LiteralExpression elmRecordTypeTagNameAsValue
                , ListExpression
                    [ ListExpression
                        (List.map2
                            (\( _, fieldNameValue ) argument ->
                                ListExpression
                                    [ LiteralExpression fieldNameValue
                                    , argument
                                    ]
                            )
                            recordFieldNamesStringAndValue
                            arguments
                        )
                    ]
                ]

        else
            FunctionApplicationExpression
                (FunctionExpression
                    (recordFieldNamesStringAndValue
                        |> List.map (\( fieldName, _ ) -> [ ( fieldName, [] ) ])
                    )
                    (ListExpression
                        [ LiteralExpression elmRecordTypeTagNameAsValue
                        , ListExpression
                            [ ListExpression
                                (List.map
                                    (\( fieldName, fieldNameValue ) ->
                                        ListExpression
                                            [ LiteralExpression fieldNameValue
                                            , ReferenceExpression fieldName
                                            ]
                                    )
                                    recordFieldNamesStringAndValue
                                )
                            ]
                        ]
                    )
                )
                arguments


shouldInlineDeclaration : String -> Expression -> Bool
shouldInlineDeclaration name expression =
    if stringStartsWithUpper name then
        True

    else
        case expression of
            LiteralExpression value ->
                estimatePineValueSize value < 50 * 1000

            _ ->
                False


listModuleTransitiveDependencies :
    List Elm.Syntax.File.File
    -> Elm.Syntax.File.File
    -> Result String (List Elm.Syntax.ModuleName.ModuleName)
listModuleTransitiveDependencies allFiles file =
    case listModuleTransitiveDependenciesExcludingModules Set.empty allFiles file of
        Err ( modulePath, error ) ->
            Err (error ++ ": " ++ String.join " -> " (List.map (String.join ".") modulePath))

        Ok ok ->
            Ok ok


listModuleTransitiveDependenciesExcludingModules :
    Set.Set (List String)
    -> List Elm.Syntax.File.File
    -> Elm.Syntax.File.File
    -> Result ( List Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.ModuleName.ModuleName)
listModuleTransitiveDependenciesExcludingModules excluded allFiles file =
    let
        (Elm.Syntax.Node.Node _ moduleDefinition) =
            file.moduleDefinition

        currentName =
            Elm.Syntax.Module.moduleName moduleDefinition

        currentDependencies =
            getDirectDependenciesFromModule file
    in
    if Set.member currentName excluded then
        Err ( [ currentName ], "Cyclic dependency" )

    else if Set.isEmpty currentDependencies then
        Ok [ currentName ]

    else
        case
            Common.resultListMapCombine
                (\currentDependency ->
                    case
                        Common.listFind
                            (\candidate ->
                                Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value candidate.moduleDefinition)
                                    == currentDependency
                            )
                            allFiles
                    of
                        Nothing ->
                            Ok []

                        Just currentDependencyFile ->
                            listModuleTransitiveDependenciesExcludingModules
                                (Set.insert currentName excluded)
                                allFiles
                                currentDependencyFile
                )
                (Set.toList currentDependencies)
        of
            Err ( moduleNames, err ) ->
                Err ( currentName :: moduleNames, err )

            Ok ok ->
                Ok (List.Extra.unique (List.concat ok ++ [ currentName ]))


getDirectDependenciesFromModule : Elm.Syntax.File.File -> Set.Set Elm.Syntax.ModuleName.ModuleName
getDirectDependenciesFromModule file =
    let
        explicit =
            List.map
                (\(Elm.Syntax.Node.Node _ importSyntax) -> Elm.Syntax.Node.value importSyntax.moduleName)
                file.imports

        (Elm.Syntax.Node.Node _ moduleDefinition) =
            file.moduleDefinition

        moduleName =
            Elm.Syntax.Module.moduleName moduleDefinition

        implicit =
            if List.member moduleName autoImportedModulesNames then
                []

            else
                autoImportedModulesNames
    in
    Set.fromList
        (explicit ++ implicit)


valueFromString : String -> Pine.Value
valueFromString string =
    Pine.ListValue
        [ elmStringTypeTagNameAsValue
        , Pine.ListValue [ Pine.valueFromString string ]
        ]


separateEnvironmentDeclarations :
    Dict.Dict String Pine.Value
    ->
        Result
            String
            { modules : Dict.Dict Elm.Syntax.ModuleName.ModuleName ( Pine.Value, ElmModuleInCompilation )
            , otherDeclarations : Dict.Dict String Pine.Value
            }
separateEnvironmentDeclarations environmentDeclarations =
    Dict.foldl
        (\declNameFlat declValue ->
            Result.andThen
                (\aggregate ->
                    if stringStartsWithUpper declNameFlat then
                        case Result.andThen parseModuleValue (getDeclarationsFromEnvironment declValue) of
                            Err err ->
                                Err ("Failed to parse module " ++ declNameFlat ++ ": " ++ err)

                            Ok moduleDeclarations ->
                                Ok
                                    { aggregate
                                        | modules =
                                            Dict.insert
                                                (String.split "." declNameFlat)
                                                ( declValue, moduleDeclarations )
                                                aggregate.modules
                                    }

                    else
                        Ok
                            { aggregate
                                | otherDeclarations =
                                    Dict.insert declNameFlat declValue aggregate.otherDeclarations
                            }
                )
        )
        (Ok { modules = Dict.empty, otherDeclarations = Dict.empty })
        environmentDeclarations


getDeclarationsFromEnvironment : Pine.Value -> Result String (Dict.Dict String Pine.Value)
getDeclarationsFromEnvironment environment =
    case environment of
        Pine.BlobValue _ ->
            Err "Is not a list but a blob"

        Pine.ListValue environmentList ->
            case
                Common.resultListMapCombine
                    (\environmentEntry ->
                        (case environmentEntry of
                            Pine.BlobValue _ ->
                                Err "Is not a list but a blob"

                            Pine.ListValue [ nameValue, namedValue ] ->
                                Pine.stringFromValue nameValue
                                    |> Result.mapError ((++) "Failed to parse string: ")
                                    |> Result.map (\name -> ( name, namedValue ))

                            Pine.ListValue list ->
                                Err
                                    ("Unexpected number of elements in environment entry list: Not 2 but "
                                        ++ String.fromInt (List.length list)
                                    )
                        )
                            |> Result.mapError ((++) "Failed to parse environment entry: ")
                    )
                    environmentList
            of
                Err err ->
                    Err err

                Ok declarations ->
                    Ok
                        (Dict.fromList
                            -- Elm Interactive allows shadowing, so ordering matters here.
                            (List.reverse declarations)
                        )


{-| Reverses the encoding implemented in emitModuleValue, parsing the Elm module from the transportable form.
-}
parseModuleValue : Dict.Dict String Pine.Value -> Result String ElmModuleInCompilation
parseModuleValue moduleValues =
    Dict.foldl
        (\declName declValue ->
            Result.andThen
                (\aggregate ->
                    if stringStartsWithUpper declName then
                        case parseTypeDeclarationFromValueTagged declValue of
                            Err err ->
                                Err err

                            Ok typeDeclaration ->
                                Ok
                                    { aggregate
                                        | typeDeclarations =
                                            Dict.insert
                                                declName
                                                typeDeclaration
                                                aggregate.typeDeclarations
                                    }

                    else
                        Ok
                            { aggregate
                                | functionDeclarations =
                                    Dict.insert
                                        declName
                                        declValue
                                        aggregate.functionDeclarations
                            }
                )
        )
        (Ok
            { functionDeclarations = Dict.empty
            , typeDeclarations = Dict.empty
            }
        )
        moduleValues


parseTypeDeclarationFromValueTagged : Pine.Value -> Result String ElmModuleTypeDeclaration
parseTypeDeclarationFromValueTagged value =
    case value of
        Pine.BlobValue _ ->
            Err "Is not a list but a blob"

        Pine.ListValue listItems ->
            case listItems of
                [ typeTagValue, functionRecord ] ->
                    case Pine.stringFromValue typeTagValue of
                        Err err ->
                            Err ("Failed to parse string: " ++ err)

                        Ok tagName ->
                            case tagName of
                                "ChoiceType" ->
                                    parseChoiceTypeFromValue functionRecord
                                        |> Result.map ElmModuleChoiceTypeDeclaration
                                        |> Result.mapError ((++) "Failed to parse choice type: ")

                                "RecordConstructor" ->
                                    parseRecordConstructorFromValue functionRecord
                                        |> Result.map ElmModuleRecordTypeDeclaration
                                        |> Result.mapError ((++) "Failed to parse record constructor: ")

                                _ ->
                                    Err ("Unknown type tag: " ++ tagName)

                _ ->
                    Err
                        ("List does not have the expected number of items: "
                            ++ String.fromInt (List.length listItems)
                        )


parseChoiceTypeFromValue : Pine.Value -> Result String ElmModuleChoiceType
parseChoiceTypeFromValue value =
    case value of
        Pine.ListValue listItems ->
            case
                Common.resultListMapCombine
                    (\tagEntry ->
                        case tagEntry of
                            Pine.BlobValue _ ->
                                Err "Is not a list but a blob"

                            Pine.ListValue [ tagNameValue, argumentCountValue ] ->
                                Pine.stringFromValue tagNameValue
                                    |> Result.mapError ((++) "Failed to parse string: ")
                                    |> Result.andThen
                                        (\tagName ->
                                            case Pine.intFromValue argumentCountValue of
                                                Err err ->
                                                    Err ("Failed to parse int: " ++ err)

                                                Ok argumentsCount ->
                                                    Ok
                                                        ( tagName
                                                        , { argumentsCount = argumentsCount }
                                                        )
                                        )

                            Pine.ListValue list ->
                                Err
                                    ("Unexpected number of elements in tag entry list: Not 2 but "
                                        ++ String.fromInt (List.length list)
                                    )
                    )
                    listItems
            of
                Err err ->
                    Err err

                Ok tags ->
                    Ok { tags = Dict.fromList tags }

        Pine.BlobValue _ ->
            Err "Is not a list but a blob"


parseRecordConstructorFromValue : Pine.Value -> Result String (List String)
parseRecordConstructorFromValue value =
    case value of
        Pine.ListValue listItems ->
            Common.resultListMapCombine
                Pine.stringFromValue
                listItems

        Pine.BlobValue _ ->
            Err "Is not a list but a blob"


stringStartsWithUpper : String -> Bool
stringStartsWithUpper string =
    case String.uncons string of
        Nothing ->
            False

        Just ( firstChar, _ ) ->
            Char.isUpper firstChar
