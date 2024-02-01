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
    , moduleNameFromSyntaxFile
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
import Result.Extra
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


type ElmModuleTypeDeclaration
    = ElmModuleChoiceTypeDeclaration ElmModuleChoiceType
    | ElmModuleRecordTypeDeclaration (List String)


type alias ElmModuleChoiceType =
    { tags : Dict.Dict String { argumentsCount : Int }
    }


pineKernelModuleName : String
pineKernelModuleName =
    "Pine_kernel"


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
                String.join "." moduleName ++ "." ++ name
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
                            , ( String.join "." (moduleName ++ [ declarationName ])
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
    case getDeclarationsFromEnvironment environmentBefore of
        Err error ->
            Err ("Failed to get declarations from environment: " ++ error)

        Ok environmentBeforeDeclarations ->
            case separateEnvironmentDeclarations environmentBeforeDeclarations of
                Err err ->
                    Err ("Failed to separate declarations from environment: " ++ err)

                Ok separateEnvironmentDeclarationsBefore ->
                    let
                        modulesNamesWithDependencies =
                            newParsedElmModules
                                |> List.map
                                    (\file ->
                                        file.parsedModule
                                            |> listModuleTransitiveDependencies (List.map .parsedModule newParsedElmModules)
                                            |> Result.mapError (Tuple.pair file)
                                            |> Result.map (Tuple.pair file)
                                    )
                    in
                    case modulesNamesWithDependencies |> Result.Extra.combine of
                        Err ( file, error ) ->
                            Err
                                ("Failed to resolve dependencies for module "
                                    ++ String.join "."
                                        (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value file.parsedModule.moduleDefinition))
                                    ++ ": "
                                    ++ error
                                )

                        Ok modulesWithDependencies ->
                            let
                                moduleNamesOrderedByDeps =
                                    modulesWithDependencies
                                        |> List.concatMap Tuple.second
                                        |> List.Extra.unique
                            in
                            moduleNamesOrderedByDeps
                                |> List.filterMap
                                    (\moduleName ->
                                        modulesWithDependencies
                                            |> Common.listFind
                                                (Tuple.first
                                                    >> .parsedModule
                                                    >> .moduleDefinition
                                                    >> Elm.Syntax.Node.value
                                                    >> Elm.Syntax.Module.moduleName
                                                    >> (==) moduleName
                                                )
                                    )
                                |> List.map Tuple.first
                                |> Ok
                                |> Result.andThen
                                    (\parsedElmFiles ->
                                        parsedElmFiles
                                            |> List.foldl
                                                (\moduleToTranslate ->
                                                    Result.andThen
                                                        (\aggregate ->
                                                            let
                                                                currentAvailableModules =
                                                                    separateEnvironmentDeclarationsBefore.modules
                                                                        |> Dict.union aggregate
                                                            in
                                                            compileElmModuleIntoNamedExports currentAvailableModules moduleToTranslate
                                                                |> Result.mapError
                                                                    ((++)
                                                                        ("Failed to compile elm module '"
                                                                            ++ String.join "." (Elm.Syntax.Node.value (moduleNameFromSyntaxFile moduleToTranslate.parsedModule))
                                                                            ++ "': "
                                                                        )
                                                                    )
                                                                |> Result.map
                                                                    (\( moduleName, moduleValue ) ->
                                                                        Dict.insert moduleName moduleValue aggregate
                                                                    )
                                                        )
                                                )
                                                (Ok Dict.empty)
                                    )
                                |> Result.map
                                    (\contextModules ->
                                        let
                                            modulesValues =
                                                contextModules
                                                    |> Dict.toList
                                                    |> List.map (Tuple.mapSecond emitModuleValue)

                                            modulesValuesWithFlatNames =
                                                modulesValues
                                                    |> List.map (Tuple.mapFirst (String.join "."))
                                        in
                                        { addedModules = modulesValues
                                        , environment =
                                            Pine.environmentFromDeclarations
                                                (Dict.toList environmentBeforeDeclarations ++ modulesValuesWithFlatNames)
                                        }
                                    )


compileElmModuleIntoNamedExports :
    Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    -> ProjectParsedElmFile
    -> Result String ( Elm.Syntax.ModuleName.ModuleName, ElmModuleInCompilation )
compileElmModuleIntoNamedExports availableModules moduleToTranslate =
    let
        moduleName =
            Elm.Syntax.Node.value (moduleNameFromSyntaxFile moduleToTranslate.parsedModule)

        moduleAliases : Dict.Dict (List String) (List String)
        moduleAliases =
            moduleToTranslate.parsedModule.imports
                |> List.filterMap
                    (Elm.Syntax.Node.value
                        >> (\imp ->
                                imp.moduleAlias
                                    |> Maybe.map
                                        (\moduleAlias ->
                                            ( Elm.Syntax.Node.value moduleAlias, Elm.Syntax.Node.value imp.moduleName )
                                        )
                           )
                    )
                |> Dict.fromList

        parsedImports =
            moduleToTranslate.parsedModule.imports
                |> List.map (Elm.Syntax.Node.value >> parseElmSyntaxImport)

        localTypeDeclarations : Dict.Dict String ElmModuleTypeDeclaration
        localTypeDeclarations =
            moduleToTranslate.parsedModule.declarations
                |> List.map Elm.Syntax.Node.value
                |> List.concatMap
                    (\declaration ->
                        case declaration of
                            Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                [ ( Elm.Syntax.Node.value choiceTypeDeclaration.name
                                  , ElmModuleChoiceTypeDeclaration
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
                                                    (\valueConstructorNode ->
                                                        let
                                                            valueConstructor =
                                                                Elm.Syntax.Node.value valueConstructorNode

                                                            valueConstructorName =
                                                                Elm.Syntax.Node.value valueConstructor.name
                                                        in
                                                        Dict.insert
                                                            valueConstructorName
                                                            { argumentsCount = List.length valueConstructor.arguments
                                                            }
                                                    )
                                                    Dict.empty
                                        }
                                  )
                                ]

                            Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
                                case aliasDeclaration.typeAnnotation of
                                    Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.Record record) ->
                                        [ ( Elm.Syntax.Node.value aliasDeclaration.name
                                          , record
                                                |> List.map (Elm.Syntax.Node.value >> Tuple.first >> Elm.Syntax.Node.value)
                                                |> ElmModuleRecordTypeDeclaration
                                          )
                                        ]

                                    Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.GenericRecord _ record) ->
                                        [ ( Elm.Syntax.Node.value aliasDeclaration.name
                                          , record
                                                |> Elm.Syntax.Node.value
                                                |> List.map (Elm.Syntax.Node.value >> Tuple.first >> Elm.Syntax.Node.value)
                                                |> ElmModuleRecordTypeDeclaration
                                          )
                                        ]

                                    _ ->
                                        []

                            _ ->
                                []
                    )
                |> Dict.fromList

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
                    compilationStackForImport.elmValuesToExposeToGlobal
                        |> Dict.filter (always ((==) moduleName >> not))
            }

        moduleExposingList : Elm.Syntax.Exposing.Exposing
        moduleExposingList =
            moduleToTranslate.parsedModule.moduleDefinition
                |> Elm.Syntax.Node.value
                |> Elm.Syntax.Module.exposingList

        redirectsForInfix : Dict.Dict String String
        redirectsForInfix =
            moduleToTranslate.parsedModule.declarations
                |> List.map Elm.Syntax.Node.value
                |> List.concatMap
                    (\declaration ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration _ ->
                                []

                            Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                                []

                            Elm.Syntax.Declaration.InfixDeclaration infixDeclaration ->
                                [ ( "(" ++ Elm.Syntax.Node.value infixDeclaration.operator ++ ")"
                                  , Elm.Syntax.Node.value infixDeclaration.function
                                  )
                                ]

                            _ ->
                                []
                    )
                |> Dict.fromList

        functionsToExposeForInfix =
            redirectsForInfix
                |> Dict.values
                |> Set.fromList

        exposeFunction functionName =
            Set.member functionName functionsToExposeForInfix
                || (case moduleExposingList of
                        Elm.Syntax.Exposing.All _ ->
                            True

                        Elm.Syntax.Exposing.Explicit explicitList ->
                            let
                                exposingList =
                                    explicitList
                                        |> List.map Elm.Syntax.Node.value
                            in
                            List.member (Elm.Syntax.Exposing.FunctionExpose functionName) exposingList
                                || List.member (Elm.Syntax.Exposing.InfixExpose functionName) exposingList
                   )

        localFunctionDeclarations : Dict.Dict String Elm.Syntax.Expression.Function
        localFunctionDeclarations =
            moduleToTranslate.parsedModule.declarations
                |> List.map Elm.Syntax.Node.value
                |> List.concatMap
                    (\declaration ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                [ ( Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                  , functionDeclaration
                                  )
                                ]

                            _ ->
                                []
                    )
                |> Dict.fromList

        localFunctionsResult : Result String (List ( String, Pine.Value ))
        localFunctionsResult =
            localFunctionDeclarations
                |> Dict.toList
                |> List.map
                    (\( functionName, functionDeclaration ) ->
                        compileElmSyntaxFunction initialCompilationStack functionDeclaration
                            |> Result.mapError ((++) ("Failed to compile function '" ++ functionName ++ "': "))
                    )
                |> Result.Extra.combine
                |> Result.map Dict.fromList
                |> Result.andThen
                    (\localFunctionDeclarationsCompiled ->
                        emitModuleFunctionDeclarations
                            initialEmitStack
                            { exposedDeclarations =
                                localFunctionDeclarationsCompiled
                                    |> Dict.filter (exposeFunction >> always)
                            , supportingDeclarations =
                                localFunctionDeclarationsCompiled
                            }
                    )
    in
    case localFunctionsResult of
        Err error ->
            Err ("Failed to compile declaration: " ++ error)

        Ok functionDeclarations ->
            let
                declarationsValuesForInfix =
                    redirectsForInfix
                        |> Dict.toList
                        |> List.filterMap
                            (\( name, function ) ->
                                functionDeclarations
                                    |> Common.listFind (Tuple.first >> (==) function)
                                    |> Maybe.map (Tuple.second >> Tuple.pair name)
                            )

                exportedFunctionDeclarations : Dict.Dict String Pine.Value
                exportedFunctionDeclarations =
                    [ List.filter (Tuple.first >> exposeFunction) functionDeclarations
                    , declarationsValuesForInfix
                    ]
                        |> List.concat
                        |> Dict.fromList
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
        localModuleName =
            importSyntax.moduleAlias
                |> Maybe.withDefault importSyntax.moduleName
                |> Elm.Syntax.Node.value

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
            case importSyntax.exposingList |> Maybe.map Elm.Syntax.Node.value of
                Nothing ->
                    Nothing

                Just (Elm.Syntax.Exposing.All _) ->
                    Just ExposingAll

                Just (Elm.Syntax.Exposing.Explicit topLevelList) ->
                    topLevelList
                        |> List.map (Elm.Syntax.Node.value >> exposedNamesFromTopLevelItem)
                        |> ExposingSelected
                        |> Just
    in
    { canonicalModuleName = Elm.Syntax.Node.value importSyntax.moduleName
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
            }

        moduleImports =
            moduleImportsFromCompilationStack
                parsedImports
                compilationStackForImport

        importedRecordConstructorsFieldsNames : Dict.Dict String (List String)
        importedRecordConstructorsFieldsNames =
            moduleImports.importedModules
                |> Dict.toList
                |> List.concatMap
                    (\( importedModuleName, importedModule ) ->
                        importedModule.typeDeclarations
                            |> Dict.toList
                            |> List.filterMap
                                (\( typeName, typeDeclaration ) ->
                                    case typeDeclaration of
                                        ElmModuleRecordTypeDeclaration fields ->
                                            Just ( typeName, fields )

                                        _ ->
                                            Nothing
                                )
                            |> List.map
                                (\( typeName, fields ) ->
                                    ( String.join "." (importedModuleName ++ [ typeName ])
                                    , fields
                                    )
                                )
                    )
                |> Dict.fromList

        importedChoiceTypeTagConstructorDeclarations : Dict.Dict String { argumentsCount : Int }
        importedChoiceTypeTagConstructorDeclarations =
            [ moduleImports.importedModules
                |> Dict.toList
                |> List.concatMap
                    (\( importedModuleName, importedModule ) ->
                        importedModule.typeDeclarations
                            |> Dict.toList
                            |> List.concatMap
                                (\( typeName, typeDeclaration ) ->
                                    case typeDeclaration of
                                        ElmModuleChoiceTypeDeclaration choiceTypeDeclaration ->
                                            choiceTypeDeclaration.tags
                                                |> Dict.toList
                                                |> List.concatMap
                                                    (\( tagName, tag ) ->
                                                        let
                                                            qualifiedName =
                                                                String.join "." (importedModuleName ++ [ tagName ])

                                                            isAutoImported =
                                                                Set.member
                                                                    qualifiedName
                                                                    elmDeclarationsToExposeToGlobalDefaultQualifiedNames

                                                            allNames =
                                                                if isAutoImported then
                                                                    [ qualifiedName, tagName ]

                                                                else
                                                                    [ qualifiedName ]
                                                        in
                                                        allNames
                                                            |> List.map
                                                                (Tuple.pair >> (|>) { argumentsCount = tag.argumentsCount })
                                                    )

                                        _ ->
                                            []
                                )
                    )
            , moduleImports.importedTypes
                |> Dict.toList
                |> List.concatMap
                    (\( typeName, typeDeclaration ) ->
                        case typeDeclaration of
                            ElmModuleChoiceTypeDeclaration choiceTypeDeclaration ->
                                Dict.toList choiceTypeDeclaration.tags

                            _ ->
                                []
                    )
            ]
                |> List.concat
                |> Dict.fromList

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
                importedRecordConstructorsFieldsNames

        choiceTypeTagConstructorDeclarations =
            Dict.union
                localTypeDeclarationsSeparate.choiceTypeTagDeclarations
                importedChoiceTypeTagConstructorDeclarations

        declarationsFromTypeAliases : Dict.Dict String (List Expression -> Expression)
        declarationsFromTypeAliases =
            declarationsFromTypeAliasesFieldsNames
                |> Dict.map (\_ -> compileElmRecordConstructor)

        declarationsFromChoiceTypes : Dict.Dict String (List Expression -> Expression)
        declarationsFromChoiceTypes =
            choiceTypeTagConstructorDeclarations
                |> Dict.map
                    (\tagName { argumentsCount } ->
                        compileElmChoiceTypeTagConstructor
                            { tagName =
                                tagName
                                    |> String.split "."
                                    |> List.reverse
                                    |> List.head
                                    |> Maybe.withDefault tagName
                            , argumentsCount = argumentsCount
                            }
                    )

        compilationStack =
            { compilationStackForImport
                | inlineableDeclarations =
                    declarationsFromChoiceTypes
                        |> Dict.union declarationsFromTypeAliases
            }

        importedModulesDeclarationsFlat : Dict.Dict String Pine.Value
        importedModulesDeclarationsFlat =
            moduleImports.importedModules
                |> Dict.foldl
                    (\moduleName importedModule modulesAggregate ->
                        importedModule.functionDeclarations
                            |> Dict.foldl
                                (\declName ->
                                    Dict.insert (String.join "." (moduleName ++ [ declName ]))
                                )
                                modulesAggregate
                    )
                    Dict.empty

        importedFunctions : Dict.Dict String Pine.Value
        importedFunctions =
            moduleImports.importedFunctions
                |> Dict.union importedModulesDeclarationsFlat

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


moduleImportsFromCompilationStack :
    List ModuleImportStatement
    -> CompilationStack
    -> ModuleImports
moduleImportsFromCompilationStack explicitImports compilation =
    let
        importedModulesImplicit =
            compilation.availableModules
                |> Dict.filter (List.member >> (|>) autoImportedModulesNames >> always)

        functionsFromImportStatement : ModuleImportStatement -> Maybe ( ElmModuleInCompilation, Dict.Dict String Pine.Value )
        functionsFromImportStatement explicitImport =
            compilation.availableModules
                |> Dict.get explicitImport.canonicalModuleName
                |> Maybe.map
                    (\availableModule ->
                        let
                            exposedDeclarations : Dict.Dict String Pine.Value
                            exposedDeclarations =
                                case explicitImport.exposingList of
                                    Nothing ->
                                        Dict.empty

                                    Just ExposingAll ->
                                        availableModule.functionDeclarations

                                    Just (ExposingSelected exposedNames) ->
                                        exposedNames
                                            |> List.concatMap
                                                (\exposedName ->
                                                    [ availableModule.functionDeclarations
                                                        |> Dict.get exposedName.name
                                                        |> Maybe.map
                                                            (Tuple.pair exposedName.name
                                                                >> List.singleton
                                                            )
                                                        |> Maybe.withDefault []
                                                    ]
                                                        |> List.concat
                                                )
                                            |> Dict.fromList
                        in
                        ( availableModule
                        , exposedDeclarations
                        )
                    )

        typesFromImportStatement : ModuleImportStatement -> Maybe (Dict.Dict String ElmModuleTypeDeclaration)
        typesFromImportStatement explicitImport =
            compilation.availableModules
                |> Dict.get explicitImport.canonicalModuleName
                |> Maybe.map
                    (\availableModule ->
                        let
                            exposedDeclarations : Dict.Dict String ElmModuleTypeDeclaration
                            exposedDeclarations =
                                case explicitImport.exposingList of
                                    Nothing ->
                                        Dict.empty

                                    Just ExposingAll ->
                                        availableModule.typeDeclarations

                                    Just (ExposingSelected exposedNames) ->
                                        exposedNames
                                            |> List.foldl
                                                (\topLevelExpose ->
                                                    availableModule.typeDeclarations
                                                        |> Dict.get topLevelExpose.name
                                                        |> Maybe.map
                                                            (mapTypeDeclarationForImport topLevelExpose
                                                                >> Dict.insert topLevelExpose.name
                                                            )
                                                        |> Maybe.withDefault identity
                                                )
                                                Dict.empty
                        in
                        exposedDeclarations
                    )

        parsedExplicitImports : List ( List String, ( ElmModuleInCompilation, Dict.Dict String Pine.Value ) )
        parsedExplicitImports =
            explicitImports
                |> List.filterMap
                    (\explicitImport ->
                        explicitImport
                            |> functionsFromImportStatement
                            |> Maybe.map (Tuple.pair explicitImport.localModuleName)
                    )

        importedFunctions : Dict.Dict String Pine.Value
        importedFunctions =
            [ compilation.elmValuesToExposeToGlobal
                |> Dict.toList
                |> List.filterMap
                    (\( name, moduleName ) ->
                        compilation.availableModules
                            |> Dict.get moduleName
                            |> Maybe.andThen (.functionDeclarations >> Dict.get name)
                            |> Maybe.map (Tuple.pair name)
                    )
            , parsedExplicitImports
                |> List.map (Tuple.second >> Tuple.second)
                |> List.map Dict.toList
                |> List.concat
            ]
                |> List.concat
                |> Dict.fromList

        importedModules : Dict.Dict (List String) ElmModuleInCompilation
        importedModules =
            parsedExplicitImports
                |> List.map (Tuple.mapSecond Tuple.first)
                |> Dict.fromList
                |> Dict.union importedModulesImplicit

        importedTypes : Dict.Dict String ElmModuleTypeDeclaration
        importedTypes =
            explicitImports
                |> List.foldl
                    (typesFromImportStatement
                        >> Maybe.map Dict.union
                        >> Maybe.withDefault identity
                    )
                    Dict.empty
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
compileElmSyntaxExpression stack elmExpression =
    case elmExpression of
        Elm.Syntax.Expression.Literal literal ->
            Ok (LiteralExpression (valueFromString literal))

        Elm.Syntax.Expression.CharLiteral char ->
            Ok (LiteralExpression (Pine.valueFromChar char))

        Elm.Syntax.Expression.Integer integer ->
            Ok (LiteralExpression (Pine.valueFromInt integer))

        Elm.Syntax.Expression.Hex integer ->
            Ok (LiteralExpression (Pine.valueFromInt integer))

        Elm.Syntax.Expression.Negation negatedElmExpression ->
            case compileElmSyntaxExpression stack (Elm.Syntax.Node.value negatedElmExpression) of
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

        Elm.Syntax.Expression.OperatorApplication operator _ leftExpr rightExpr ->
            let
                orderedElmExpression =
                    mapExpressionForOperatorPrecedence elmExpression
            in
            if orderedElmExpression /= elmExpression then
                compileElmSyntaxExpression stack orderedElmExpression

            else
                compileElmSyntaxExpression stack (Elm.Syntax.Node.value leftExpr)
                    |> Result.mapError ((++) "Failed to compile left expression: ")
                    |> Result.andThen
                        (\leftExpression ->
                            compileElmSyntaxExpression stack (Elm.Syntax.Node.value rightExpr)
                                |> Result.mapError ((++) "Failed to compile right expression: ")
                                |> Result.andThen
                                    (\rightExpression ->
                                        compileElmFunctionOrValueLookup ( [], "(" ++ operator ++ ")" ) stack
                                            |> Result.map
                                                (\operationFunction ->
                                                    FunctionApplicationExpression
                                                        operationFunction
                                                        [ leftExpression, rightExpression ]
                                                )
                                    )
                        )
                    |> Result.mapError ((++) ("Failed to compile OperatorApplication '" ++ operator ++ "': "))

        Elm.Syntax.Expression.PrefixOperator operator ->
            compileElmFunctionOrValueLookup ( [], "(" ++ operator ++ ")" ) stack

        Elm.Syntax.Expression.IfBlock elmCondition elmExpressionIfTrue elmExpressionIfFalse ->
            case compileElmSyntaxExpression stack (Elm.Syntax.Node.value elmCondition) of
                Err error ->
                    Err ("Failed to compile Elm condition: " ++ error)

                Ok conditionExpression ->
                    case compileElmSyntaxExpression stack (Elm.Syntax.Node.value elmExpressionIfTrue) of
                        Err error ->
                            Err ("Failed to compile Elm expressionIfTrue: " ++ error)

                        Ok expressionIfTrue ->
                            case compileElmSyntaxExpression stack (Elm.Syntax.Node.value elmExpressionIfFalse) of
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

        Elm.Syntax.Expression.ParenthesizedExpression parenthesizedExpression ->
            compileElmSyntaxExpression stack (Elm.Syntax.Node.value parenthesizedExpression)

        Elm.Syntax.Expression.ListExpr listExpression ->
            listExpression
                |> List.map (Elm.Syntax.Node.value >> compileElmSyntaxExpression stack)
                |> Result.Extra.combine
                |> Result.map ListExpression

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            compileElmSyntaxCaseBlock stack caseBlock

        Elm.Syntax.Expression.LambdaExpression lambdaExpression ->
            compileElmSyntaxLambda stack lambdaExpression

        Elm.Syntax.Expression.RecordExpr recordExpr ->
            recordExpr
                |> List.map Elm.Syntax.Node.value
                |> compileElmSyntaxRecord stack

        Elm.Syntax.Expression.TupledExpression tupleElements ->
            tupleElements
                |> List.map (Elm.Syntax.Node.value >> compileElmSyntaxExpression stack)
                |> Result.Extra.combine
                |> Result.map ListExpression

        Elm.Syntax.Expression.RecordAccess expressionNode nameNode ->
            compileElmSyntaxRecordAccess
                stack
                (Elm.Syntax.Node.value nameNode)
                (Elm.Syntax.Node.value expressionNode)

        Elm.Syntax.Expression.RecordAccessFunction accessSyntax ->
            let
                fieldName =
                    if String.startsWith "." accessSyntax then
                        String.dropLeft 1 accessSyntax

                    else
                        accessSyntax
            in
            Ok (compileElmSyntaxRecordAccessFunction fieldName)

        Elm.Syntax.Expression.RecordUpdateExpression recordNameNode settersNodes ->
            compileElmSyntaxRecordUpdate
                stack
                (List.map (Elm.Syntax.Node.value >> Tuple.mapBoth Elm.Syntax.Node.value Elm.Syntax.Node.value) settersNodes)
                (Elm.Syntax.Node.value recordNameNode)

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
    case argumentsElmSyntax |> List.map (compileElmSyntaxExpression stack) |> Result.Extra.combine of
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
                    if functionModuleName == [ pineKernelModuleName ] then
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

                    else
                        let
                            functionFlatName =
                                String.join "." (functionModuleName ++ [ functionLocalName ])
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

                _ ->
                    continueWithDefaultApplication ()


compileElmSyntaxLetBlock :
    CompilationStack
    -> Elm.Syntax.Expression.LetBlock
    -> Result String Expression
compileElmSyntaxLetBlock stackBefore letBlock =
    letBlock.declarations
        |> List.concatMap
            (\letDeclaration ->
                case Elm.Syntax.Node.value letDeclaration of
                    Elm.Syntax.Expression.LetFunction _ ->
                        []

                    Elm.Syntax.Expression.LetDestructuring (Elm.Syntax.Node.Node _ pattern) (Elm.Syntax.Node.Node _ destructuredExpressionElm) ->
                        destructuredExpressionElm
                            |> compileElmSyntaxExpression stackBefore
                            |> Result.andThen
                                (\destructuredExpression ->
                                    pattern
                                        |> compileElmSyntaxPattern
                                        |> Result.map
                                            (.declarations
                                                >> List.map
                                                    (\( declName, deconsExpr ) ->
                                                        ( declName
                                                        , destructuredExpression
                                                            |> expressionForDeconstructions deconsExpr
                                                            |> applicableDeclarationFromConstructorExpression
                                                        )
                                                    )
                                            )
                                )
                            |> Result.Extra.unpack (Err >> List.singleton) (List.map Ok)
            )
        |> Result.Extra.combine
        |> Result.andThen
            (\newAvailableDeclarations ->
                let
                    stack =
                        { stackBefore
                            | inlineableDeclarations =
                                stackBefore.inlineableDeclarations
                                    |> Dict.union (Dict.fromList newAvailableDeclarations)
                        }

                    letEntriesResults =
                        letBlock.declarations
                            |> List.map (Elm.Syntax.Node.value >> compileElmSyntaxLetDeclaration stack)
                in
                case letEntriesResults |> Result.Extra.combine of
                    Err error ->
                        Err ("Failed to compile declaration in let block: " ++ error)

                    Ok letEntries ->
                        compileElmSyntaxExpression stack (Elm.Syntax.Node.value letBlock.expression)
                            |> Result.map
                                (DeclarationBlockExpression (Dict.fromList (List.concat letEntries)))
            )


compileElmSyntaxLetDeclaration :
    CompilationStack
    -> Elm.Syntax.Expression.LetDeclaration
    -> Result String (List ( String, Expression ))
compileElmSyntaxLetDeclaration stack declaration =
    case declaration of
        Elm.Syntax.Expression.LetFunction letFunction ->
            compileElmSyntaxFunction stack letFunction
                |> Result.map List.singleton

        Elm.Syntax.Expression.LetDestructuring patternNode expressionNode ->
            compileElmSyntaxExpression stack (Elm.Syntax.Node.value expressionNode)
                |> Result.andThen
                    (\compiledExpression ->
                        compileElmSyntaxPattern (Elm.Syntax.Node.value patternNode)
                            |> Result.map .declarations
                            |> Result.mapError ((++) "Failed destructuring in let block: ")
                            |> Result.map (List.map (Tuple.mapSecond (expressionForDeconstructions >> (|>) compiledExpression)))
                    )


compileElmSyntaxFunction :
    CompilationStack
    -> Elm.Syntax.Expression.Function
    -> Result String ( String, Expression )
compileElmSyntaxFunction stack function =
    compileElmSyntaxFunctionWithoutName stack
        { arguments = (Elm.Syntax.Node.value function.declaration).arguments |> List.map Elm.Syntax.Node.value
        , expression = Elm.Syntax.Node.value (Elm.Syntax.Node.value function.declaration).expression
        }
        |> Result.map
            (\functionWithoutName ->
                ( Elm.Syntax.Node.value (Elm.Syntax.Node.value function.declaration).name
                , functionWithoutName
                )
            )


compileElmSyntaxFunctionWithoutName :
    CompilationStack
    -> ElmFunctionDeclarationStruct
    -> Result String Expression
compileElmSyntaxFunctionWithoutName stackBefore function =
    case
        function.arguments
            |> List.map (compileElmSyntaxPattern >> Result.map .declarations)
            |> Result.Extra.combine
    of
        Err error ->
            Err ("Failed to compile function parameter pattern: " ++ error)

        Ok argumentsDeconstructDeclarationsBuilders ->
            function.expression
                |> compileElmSyntaxExpression stackBefore
                |> Result.map (FunctionExpression argumentsDeconstructDeclarationsBuilders)


compileElmSyntaxLambda :
    CompilationStack
    -> Elm.Syntax.Expression.Lambda
    -> Result String Expression
compileElmSyntaxLambda stack lambda =
    compileElmSyntaxFunctionWithoutName stack
        { arguments = lambda.args |> List.map Elm.Syntax.Node.value
        , expression = Elm.Syntax.Node.value lambda.expression
        }


compileElmSyntaxRecord :
    CompilationStack
    -> List Elm.Syntax.Expression.RecordSetter
    -> Result String Expression
compileElmSyntaxRecord stack recordSetters =
    recordSetters
        |> List.map (Tuple.mapFirst Elm.Syntax.Node.value)
        |> List.sortBy Tuple.first
        |> List.map
            (\( fieldName, fieldExpressionNode ) ->
                case compileElmSyntaxExpression stack (Elm.Syntax.Node.value fieldExpressionNode) of
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
        |> Result.Extra.combine
        |> Result.map
            (\fieldsExpressions ->
                ListExpression
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
    compileElmSyntaxExpression stack recordElmExpression
        |> Result.mapError ((++) "Failed to compile record expression: ")
        |> Result.map (compileRecordAccessExpression fieldName)


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
        [ [ ( "record_param", [] ) ] ]
        (compileRecordAccessExpression
            fieldName
            (ReferenceExpression "record_param")
        )


compileElmSyntaxRecordUpdate :
    CompilationStack
    -> List ( String, Elm.Syntax.Expression.Expression )
    -> String
    -> Result String Expression
compileElmSyntaxRecordUpdate stack setters recordName =
    setters
        |> List.map
            (\( fieldName, fieldExpr ) ->
                compileElmSyntaxExpression stack fieldExpr
                    |> Result.mapError ((++) ("Failed to compile record update field '" ++ fieldName ++ "': "))
                    |> Result.map (Tuple.pair fieldName)
            )
        |> Result.Extra.combine
        |> Result.map
            (\settersExpressions ->
                PineFunctionApplicationExpression
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
    case compileElmSyntaxExpression stack (Elm.Syntax.Node.value caseBlock.expression) of
        Err error ->
            Err ("Failed to compile case block expression: " ++ error)

        Ok expression ->
            case
                caseBlock.cases
                    |> List.map (compileElmSyntaxCaseBlockCase stack expression)
                    |> Result.Extra.combine
            of
                Err error ->
                    Err ("Failed to compile case in case-of block: " ++ error)

                Ok cases ->
                    let
                        conditionalFromCase deconstructedCase nextBlockExpression =
                            deconstructedCase.conditionExpressions
                                |> List.foldl
                                    (\conditionExpression nextConditionExpression ->
                                        ConditionalExpression
                                            { condition = conditionExpression
                                            , ifTrue = nextConditionExpression
                                            , ifFalse = nextBlockExpression
                                            }
                                    )
                                    deconstructedCase.thenExpression
                    in
                    Ok
                        (List.foldr
                            conditionalFromCase
                            (ListExpression
                                [ LiteralExpression (Pine.valueFromString "Error in case-of block: No matching branch.")
                                , expression
                                ]
                            )
                            cases
                        )


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
compileElmSyntaxCaseBlockCase stackBefore caseBlockValueExpression ( elmPatternNode, elmExpression ) =
    case compileElmSyntaxPattern (Elm.Syntax.Node.value elmPatternNode) of
        Err error ->
            Err error

        Ok deconstruction ->
            let
                deconstructionDeclarations =
                    deconstruction.declarations
                        |> List.map
                            (Tuple.mapSecond
                                (expressionForDeconstructions
                                    >> (|>) caseBlockValueExpression
                                )
                            )
                        |> Dict.fromList

                stack =
                    { stackBefore
                        | inlineableDeclarations =
                            stackBefore.inlineableDeclarations
                                |> Dict.union
                                    (Dict.map (always applicableDeclarationFromConstructorExpression)
                                        deconstructionDeclarations
                                    )
                    }
            in
            elmExpression
                |> Elm.Syntax.Node.value
                |> compileElmSyntaxExpression stack
                |> Result.map
                    (\expression ->
                        { conditionExpressions = deconstruction.conditionExpressions caseBlockValueExpression
                        , thenExpression =
                            if deconstruction.declarations == [] then
                                expression

                            else
                                DeclarationBlockExpression
                                    deconstructionDeclarations
                                    expression
                        }
                    )


compileElmSyntaxPattern :
    Elm.Syntax.Pattern.Pattern
    ->
        Result
            String
            { conditionExpressions : Expression -> List Expression
            , declarations : List ( String, List Deconstruction )
            }
compileElmSyntaxPattern elmPattern =
    let
        continueWithOnlyEqualsCondition valueToCompare =
            Ok
                { conditionExpressions =
                    \deconstructedExpression ->
                        [ equalCondition [ deconstructedExpression, valueToCompare ] ]
                , declarations = []
                }

        conditionsAndDeclarationsFromItemPattern itemIndex =
            compileElmSyntaxPattern
                >> Result.map
                    (\listElementResult ->
                        { conditions =
                            listItemFromIndexExpression itemIndex
                                >> listElementResult.conditionExpressions
                        , declarations =
                            listElementResult.declarations
                                |> List.map (Tuple.mapSecond ((::) (ListItemDeconstruction itemIndex)))
                        }
                    )

        continueWithListOrTupleItems listItems =
            listItems
                |> List.map Elm.Syntax.Node.value
                |> List.indexedMap conditionsAndDeclarationsFromItemPattern
                |> Result.Extra.combine
                |> Result.map
                    (\itemsResults ->
                        let
                            matchesLengthCondition : Expression -> Expression
                            matchesLengthCondition =
                                \deconstructedExpression ->
                                    equalCondition
                                        [ LiteralExpression (Pine.valueFromInt (List.length listItems))
                                        , countListElementsExpression deconstructedExpression
                                        ]

                            conditionExpressions : Expression -> List Expression
                            conditionExpressions =
                                \deconstructedExpression ->
                                    matchesLengthCondition deconstructedExpression
                                        :: List.concatMap (.conditions >> (|>) deconstructedExpression) itemsResults
                        in
                        { conditionExpressions = conditionExpressions
                        , declarations = itemsResults |> List.concatMap .declarations
                        }
                    )
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

        Elm.Syntax.Pattern.UnConsPattern unconsLeft unconsRight ->
            unconsLeft
                |> Elm.Syntax.Node.value
                |> compileElmSyntaxPattern
                |> Result.andThen
                    (\leftSide ->
                        unconsRight
                            |> Elm.Syntax.Node.value
                            |> compileElmSyntaxPattern
                            |> Result.map
                                (\rightSide ->
                                    let
                                        conditionExpressions =
                                            \deconstructedExpression ->
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
                                                    |> List.concat

                                        declarations =
                                            [ leftSide.declarations
                                                |> List.map (Tuple.mapSecond ((::) (ListItemDeconstruction 0)))
                                            , rightSide.declarations
                                                |> List.map (Tuple.mapSecond ((::) (SkipItemsDeconstruction 1)))
                                            ]
                                                |> List.concat
                                    in
                                    { conditionExpressions = conditionExpressions
                                    , declarations = declarations
                                    }
                                )
                    )

        Elm.Syntax.Pattern.NamedPattern qualifiedName choiceTypeArgumentPatterns ->
            choiceTypeArgumentPatterns
                |> List.map Elm.Syntax.Node.value
                |> List.indexedMap
                    (\argIndex argPattern ->
                        conditionsAndDeclarationsFromItemPattern argIndex argPattern
                            |> Result.mapError
                                ((++)
                                    ("Failed for named pattern argument "
                                        ++ String.fromInt argIndex
                                        ++ ": "
                                    )
                                )
                    )
                |> Result.Extra.combine
                |> Result.map
                    (\itemsResults ->
                        let
                            conditionExpressions : Expression -> List Expression
                            conditionExpressions =
                                \deconstructedExpression ->
                                    let
                                        matchingTagCondition =
                                            case Dict.get qualifiedName.name elmDeclarationsOverridesExpressions of
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

                                        argumentsConditions =
                                            itemsResults
                                                |> List.concatMap
                                                    (.conditions
                                                        >> (|>) (listItemFromIndexExpression 1 deconstructedExpression)
                                                    )
                                    in
                                    matchingTagCondition :: argumentsConditions

                            declarations =
                                itemsResults
                                    |> List.concatMap .declarations
                                    |> List.map (Tuple.mapSecond ((::) (ListItemDeconstruction 1)))
                        in
                        { conditionExpressions = conditionExpressions
                        , declarations = declarations
                        }
                    )

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
                        |> List.map Elm.Syntax.Node.value
                        |> List.map
                            (\fieldName ->
                                ( fieldName
                                , [ Pine.DecodeAndEvaluateExpression
                                        { expression = Pine.LiteralExpression pineFunctionForRecordAccessAsValue
                                        , environment =
                                            Pine.ListExpression
                                                [ Pine.EnvironmentExpression
                                                , Pine.LiteralExpression (Pine.valueFromString fieldName)
                                                ]
                                        }
                                        |> PineFunctionApplicationDeconstruction
                                  ]
                                )
                            )
                }

        Elm.Syntax.Pattern.AsPattern (Elm.Syntax.Node.Node _ aliasedPattern) (Elm.Syntax.Node.Node _ alias) ->
            compileElmSyntaxPattern aliasedPattern
                |> Result.map
                    (\aliasedResult ->
                        { aliasedResult
                            | declarations = ( alias, [] ) :: aliasedResult.declarations
                        }
                    )

        Elm.Syntax.Pattern.ParenthesizedPattern parenthesized ->
            compileElmSyntaxPattern (Elm.Syntax.Node.value parenthesized)

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                { conditionExpressions = always []
                , declarations = []
                }

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "Unsupported type of pattern: FloatPattern"

        Elm.Syntax.Pattern.HexPattern _ ->
            Err "Unsupported type of pattern: HexPattern"


mapExpressionForOperatorPrecedence : Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
mapExpressionForOperatorPrecedence originalExpression =
    case originalExpression of
        Elm.Syntax.Expression.OperatorApplication operator direction leftExpr rightExpr ->
            let
                operatorPriority =
                    operatorPrecendencePriority |> Dict.get operator |> Maybe.withDefault 0

                mappedLeftExpr =
                    Elm.Syntax.Node.Node (Elm.Syntax.Node.range leftExpr)
                        (mapExpressionForOperatorPrecedence (Elm.Syntax.Node.value leftExpr))

                mappedRightExpr =
                    Elm.Syntax.Node.Node (Elm.Syntax.Node.range rightExpr)
                        (mapExpressionForOperatorPrecedence (Elm.Syntax.Node.value rightExpr))

                orderedLeft =
                    case Elm.Syntax.Node.value mappedLeftExpr of
                        Elm.Syntax.Expression.OperatorApplication leftOperator _ leftLeftExpr leftRightExpr ->
                            let
                                operatorLeftPriority =
                                    operatorPrecendencePriority |> Dict.get leftOperator |> Maybe.withDefault 0

                                areStillOrderedBySyntaxRange =
                                    compareLocations
                                        (Elm.Syntax.Node.range leftExpr).start
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
                                            (Elm.Syntax.Range.combine [ Elm.Syntax.Node.range leftRightExpr, Elm.Syntax.Node.range rightExpr ])
                                            (Elm.Syntax.Expression.OperatorApplication operator direction leftRightExpr rightExpr)
                                        )
                                    )

                            else
                                originalExpression

                        _ ->
                            originalExpression
            in
            if mappedLeftExpr /= leftExpr || mappedRightExpr /= rightExpr then
                mapExpressionForOperatorPrecedence (Elm.Syntax.Expression.OperatorApplication operator direction mappedLeftExpr mappedRightExpr)

            else
                case Elm.Syntax.Node.value mappedRightExpr of
                    Elm.Syntax.Expression.OperatorApplication rightOperator _ rightLeftExpr rightRightExpr ->
                        let
                            operatorRightPriority =
                                operatorPrecendencePriority |> Dict.get rightOperator |> Maybe.withDefault 0

                            areStillOrderedBySyntaxRange =
                                compareLocations
                                    (Elm.Syntax.Node.range leftExpr).start
                                    (Elm.Syntax.Node.range rightLeftExpr).start
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
                                        (Elm.Syntax.Range.combine [ Elm.Syntax.Node.range leftExpr, Elm.Syntax.Node.range rightLeftExpr ])
                                        (Elm.Syntax.Expression.OperatorApplication operator direction leftExpr rightLeftExpr)
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
expressionForDeconstructions =
    List.map expressionForDeconstruction
        >> List.foldr (>>) identity


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
            listItemFromIndexExpression_Pine 0 Pine.EnvironmentExpression

        recordFieldsExpression =
            pineKernel_ListHead_Pine (listItemFromIndexExpression_Pine 1 recordExpression)

        fieldsUpdatesExpression =
            listItemFromIndexExpression_Pine 1 Pine.EnvironmentExpression

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
                    [ Pine.DecodeAndEvaluateExpression
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
        , ifFalse = Pine.ListExpression []
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
            listItemFromIndexExpression_Pine 0 Pine.EnvironmentExpression

        fieldPairsLocalExpression : Pine.Expression
        fieldPairsLocalExpression =
            listItemFromIndexExpression_Pine 1 Pine.EnvironmentExpression

        processedFieldsLocalExpression : Pine.Expression
        processedFieldsLocalExpression =
            listItemFromIndexExpression_Pine 2 Pine.EnvironmentExpression

        remainingFieldsLocalExpression : Pine.Expression
        remainingFieldsLocalExpression =
            listItemFromIndexExpression_Pine 3 Pine.EnvironmentExpression

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
                , remainingFieldsLocalExpression
                ]
        , ifTrue = processedFieldsLocalExpression
        , ifFalse =
            Pine.ConditionalExpression
                { condition =
                    equalCondition_Pine
                        [ listItemFromIndexExpression_Pine 0 remainingFieldsNextLocalExpression
                        , firstFieldNameLocalExpression
                        ]
                , ifTrue =
                    Pine.DecodeAndEvaluateExpression
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
                    Pine.DecodeAndEvaluateExpression
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


pineFunctionForRecordAccessAsValue : Pine.Value
pineFunctionForRecordAccessAsValue =
    Pine.encodeExpressionAsValue pineFunctionForRecordAccess


pineFunctionForRecordAccess : Pine.Expression
pineFunctionForRecordAccess =
    let
        recordExpression =
            listItemFromIndexExpression_Pine 0 Pine.EnvironmentExpression

        fieldNameLocalExpression =
            listItemFromIndexExpression_Pine 1 Pine.EnvironmentExpression

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
            Pine.DecodeAndEvaluateExpression
                { expression = Pine.LiteralExpression recursiveFunctionToLookupFieldInRecordAsValue
                , environment =
                    Pine.ListExpression
                        [ Pine.LiteralExpression recursiveFunctionToLookupFieldInRecordAsValue
                        , fieldNameLocalExpression
                        , recordFieldsExpression
                        ]
                }
        , ifFalse = Pine.ListExpression []
        }


recursiveFunctionToLookupFieldInRecordAsValue : Pine.Value
recursiveFunctionToLookupFieldInRecordAsValue =
    Pine.encodeExpressionAsValue recursiveFunctionToLookupFieldInRecord


recursiveFunctionToLookupFieldInRecord : Pine.Expression
recursiveFunctionToLookupFieldInRecord =
    let
        selfFunctionLocalExpression =
            listItemFromIndexExpression_Pine 0 Pine.EnvironmentExpression

        fieldNameLocalExpression =
            listItemFromIndexExpression_Pine 1 Pine.EnvironmentExpression

        remainingFieldsLocalExpression =
            listItemFromIndexExpression_Pine 2 Pine.EnvironmentExpression

        continueWithRemainingExpression =
            Pine.DecodeAndEvaluateExpression
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
        , ifTrue = continueWithRemainingExpression
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
            String.join "." (moduleName ++ [ name ])
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
            String.join "." (canonicalModuleName ++ [ nameInModule ])

        continueWithDefault () =
            case compilation.availableModules |> Dict.get canonicalModuleName of
                Nothing ->
                    Err
                        ("Did not find module '"
                            ++ String.join "." canonicalModuleName
                            ++ "'. There are "
                            ++ (String.fromInt (Dict.size compilation.availableModules)
                                    ++ " declarations in this scope: "
                                    ++ String.join ", " (List.map (String.join ".") (Dict.keys compilation.availableModules))
                               )
                        )

                Just moduleValue ->
                    case Dict.get nameInModule moduleValue.functionDeclarations of
                        Nothing ->
                            case Dict.get flatName compilation.inlineableDeclarations of
                                Just applicableDeclaration ->
                                    Ok (applicableDeclaration [])

                                Nothing ->
                                    Err
                                        ("Did not find '"
                                            ++ nameInModule
                                            ++ "' in module '"
                                            ++ String.join "." canonicalModuleName
                                            ++ "'. There are "
                                            ++ String.fromInt (Dict.size moduleValue.functionDeclarations)
                                            ++ " function declarations available in that module: "
                                            ++ String.join ", " (Dict.keys moduleValue.functionDeclarations)
                                        )

                        Just declarationValue ->
                            Ok (LiteralExpression declarationValue)
    in
    case Dict.get canonicalModuleName getDeclarationValueFromCompilationOverrides of
        Nothing ->
            continueWithDefault ()

        Just overrides ->
            case Dict.get nameInModule overrides of
                Just overrideValue ->
                    Result.map LiteralExpression overrideValue

                Nothing ->
                    continueWithDefault ()


getDeclarationValueFromCompilationOverrides : Dict.Dict (List String) (Dict.Dict String (Result String Pine.Value))
getDeclarationValueFromCompilationOverrides =
    [ ( [ "Debug" ]
      , [ ( "log"
            -- TODO: mapping for Debug.log so we can get messages.
          , FunctionExpression
                [ [ ( "message", [] ) ], [ ( "payload", [] ) ] ]
                (ReferenceExpression "payload")
                |> FirCompiler.emitExpression
                    { importedFunctions = Dict.empty
                    , declarationsDependencies = Dict.empty
                    , environmentFunctions = []
                    , environmentDeconstructions = Dict.empty
                    }
                |> Result.andThen evaluateAsIndependentExpression
          )
        , ( "toString"
            -- TODO: mapping for Debug.toString
          , FunctionExpression
                [ [ ( "elm_value", [] ) ] ]
                (LiteralExpression (valueFromString "Debug.toString is not implemented yet"))
                |> FirCompiler.emitExpression
                    { importedFunctions = Dict.empty
                    , declarationsDependencies = Dict.empty
                    , environmentFunctions = []
                    , environmentDeconstructions = Dict.empty
                    }
                |> Result.andThen evaluateAsIndependentExpression
          )
        ]
            |> Dict.fromList
      )
    ]
        |> Dict.fromList


compileLookupForInlineableDeclaration : ( List String, String ) -> Expression -> Expression
compileLookupForInlineableDeclaration ( moduleName, name ) expression =
    let
        fusedName =
            String.join "." (moduleName ++ [ name ])
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
    (emittedFunctions ++ typeDescriptions)
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

        importedFunctionsNotShadowed : Dict.Dict String Pine.Value
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

        usedImports : Dict.Dict String Pine.Value
        usedImports =
            Dict.filter
                (\declName _ -> Set.member declName aggregateTransitiveDependencies)
                importedFunctionsNotShadowed

        usedImportsAvailableEmittedFunctions : List ( FirCompiler.EnvironmentFunctionEntry, Pine.Value )
        usedImportsAvailableEmittedFunctions =
            Dict.toList usedImports
                |> List.map
                    (\( functionName, functionValue ) ->
                        ( { functionName = functionName
                          , expectedEnvironmentFunctions = []
                          , parameterCount = 0
                          }
                        , Pine.encodeExpressionAsValue (Pine.LiteralExpression functionValue)
                        )
                    )

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
                                    (alreadyEmitted ++ [ emittedDomain ])
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
                    usedImportsAvailableEmittedFunctions ++ availableFunctionsValues

                additionalImports : Set.Set String
                additionalImports =
                    FirCompiler.getTransitiveDependencies
                        declarationsDirectDependencies
                        currentRecursionDomain

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
            in
            FirCompiler.emitDeclarationBlock
                emitStack
                { availableEmittedFunctions = availableEmittedFunctionsIncludingImports }
                recursionDomainDeclarationsInBlock
                { closureCaptures = []
                , additionalImports = additionalImports
                }
                |> Result.andThen
                    (\( blockEmitStack, blockDeclarationsEmitted ) ->
                        recursionDomainDeclarations
                            |> Dict.toList
                            |> List.map
                                (\( declarationName, declarationExpression ) ->
                                    let
                                        getFunctionInnerExpressionFromIndex : Int -> Pine.Expression
                                        getFunctionInnerExpressionFromIndex declarationIndex =
                                            let
                                                getEnvFunctionsExpression =
                                                    Pine.EnvironmentExpression
                                                        |> listItemFromIndexExpression_Pine 0
                                            in
                                            Pine.LiteralExpression
                                                (Pine.encodeExpressionAsValue
                                                    (Pine.DecodeAndEvaluateExpression
                                                        { expression =
                                                            FirCompiler.listItemFromIndexExpression_Pine
                                                                declarationIndex
                                                                getEnvFunctionsExpression
                                                        , environment = Pine.EnvironmentExpression
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
                                                        , functionParameterCount = declMatch.parameterCount
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
                            |> Result.Extra.combine
                            |> Result.mapError
                                (\err ->
                                    "Failed in recursion domain: "
                                        ++ String.join ", " (Set.toList currentRecursionDomain)
                                        ++ ": "
                                        ++ err
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
                                                          , expectedEnvironmentFunctions = expectedEnvironmentFunctions
                                                          }
                                                        , innerExpression
                                                        )
                                                    )

                                        emittedDeclarations : List ( FirCompiler.EnvironmentFunctionEntry, ( Pine.Expression, Pine.Value ) )
                                        emittedDeclarations =
                                            emittedDeclarationsFromBlock
                                                ++ List.filter
                                                    (\( { functionName }, _ ) ->
                                                        not (Set.member functionName emittedDeclarationsFromBlockNames)
                                                    )
                                                    emittedDeclarationsFromExposed

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


compileElmChoiceTypeTagConstructor : { tagName : String, argumentsCount : Int } -> (List Expression -> Expression)
compileElmChoiceTypeTagConstructor { tagName, argumentsCount } =
    let
        tagNameAsValue =
            Pine.valueFromString tagName

        genericContructorValue =
            Tuple.second
                (compileElmChoiceTypeTagConstructorValue { tagName = tagName, argumentsCount = argumentsCount })
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
                , Pine.ListExpression [ Pine.EnvironmentExpression ]
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
                                , Pine.EnvironmentExpression
                                ]
                            , Pine.EnvironmentExpression
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
                            Pine.EnvironmentExpression
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
    listModuleTransitiveDependenciesExcludingModules Set.empty allFiles file
        |> Result.mapError
            (\( modulePath, error ) -> error ++ ": " ++ String.join " -> " (List.map (String.join ".") modulePath))


listModuleTransitiveDependenciesExcludingModules :
    Set.Set (List String)
    -> List Elm.Syntax.File.File
    -> Elm.Syntax.File.File
    -> Result ( List Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.ModuleName.ModuleName)
listModuleTransitiveDependenciesExcludingModules excluded allFiles file =
    let
        currentName =
            Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value file.moduleDefinition)

        currentDependencies =
            getDirectDependenciesFromModule file
    in
    if Set.member currentName excluded then
        Err ( [ currentName ], "Cyclic dependency" )

    else if Set.isEmpty currentDependencies then
        Ok [ currentName ]

    else
        currentDependencies
            |> Set.toList
            |> List.map
                (\currentDependency ->
                    case
                        allFiles
                            |> Common.listFind
                                (.moduleDefinition
                                    >> Elm.Syntax.Node.value
                                    >> Elm.Syntax.Module.moduleName
                                    >> (==) currentDependency
                                )
                    of
                        Nothing ->
                            Ok []

                        Just currentDependencyFile ->
                            listModuleTransitiveDependenciesExcludingModules
                                (Set.insert currentName excluded)
                                allFiles
                                currentDependencyFile
                )
            |> Result.Extra.combine
            |> Result.mapError (Tuple.mapFirst ((::) currentName))
            |> Result.map (List.concat >> (++) >> (|>) [ currentName ] >> List.Extra.unique)


getDirectDependenciesFromModule : Elm.Syntax.File.File -> Set.Set Elm.Syntax.ModuleName.ModuleName
getDirectDependenciesFromModule file =
    let
        explicit =
            file.imports
                |> List.map (Elm.Syntax.Node.value >> .moduleName >> Elm.Syntax.Node.value)

        implicit =
            if List.member (Elm.Syntax.Node.value (moduleNameFromSyntaxFile file)) autoImportedModulesNames then
                []

            else
                autoImportedModulesNames
    in
    explicit
        ++ implicit
        |> Set.fromList


valueFromString : String -> Pine.Value
valueFromString string =
    Pine.ListValue
        [ elmStringTypeTagNameAsValue
        , Pine.ListValue [ Pine.valueFromString string ]
        ]


moduleNameFromSyntaxFile : Elm.Syntax.File.File -> Elm.Syntax.Node.Node (List String)
moduleNameFromSyntaxFile file =
    case Elm.Syntax.Node.value file.moduleDefinition of
        Elm.Syntax.Module.NormalModule normalModule ->
            normalModule.moduleName

        Elm.Syntax.Module.PortModule portModule ->
            portModule.moduleName

        Elm.Syntax.Module.EffectModule effectModule ->
            effectModule.moduleName


separateEnvironmentDeclarations :
    Dict.Dict String Pine.Value
    ->
        Result
            String
            { modules : Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
            , otherDeclarations : Dict.Dict String Pine.Value
            }
separateEnvironmentDeclarations environmentDeclarations =
    Dict.foldl
        (\declNameFlat declValue ->
            Result.andThen
                (\aggregate ->
                    if stringStartsWithUpper declNameFlat then
                        getDeclarationsFromEnvironment declValue
                            |> Result.andThen parseModuleValue
                            |> Result.mapError ((++) ("Failed to parse module " ++ declNameFlat))
                            |> Result.map
                                (\moduleDeclarations ->
                                    { aggregate
                                        | modules =
                                            Dict.insert
                                                (String.split "." declNameFlat)
                                                moduleDeclarations
                                                aggregate.modules
                                    }
                                )

                    else
                        { aggregate
                            | otherDeclarations =
                                Dict.insert declNameFlat declValue aggregate.otherDeclarations
                        }
                            |> Ok
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
            environmentList
                |> List.map
                    (\environmentEntry ->
                        (case environmentEntry of
                            Pine.BlobValue _ ->
                                Err "Is not a list but a blob"

                            Pine.ListValue [ nameValue, namedValue ] ->
                                Pine.stringFromValue nameValue
                                    |> Result.mapError ((++) "Failed to decode string: ")
                                    |> Result.map (\name -> ( name, namedValue ))

                            Pine.ListValue list ->
                                Err
                                    ("Unexpected number of elements in environment entry list: Not 2 but "
                                        ++ String.fromInt (List.length list)
                                    )
                        )
                            |> Result.mapError ((++) "Failed to decode environment entry: ")
                    )
                |> Result.Extra.combine
                |> Result.map (List.reverse >> Dict.fromList)


{-| Reverses the encoding implemented in emitModuleValue, parsing the Elm module from the transportable form.
-}
parseModuleValue : Dict.Dict String Pine.Value -> Result String ElmModuleInCompilation
parseModuleValue moduleValues =
    moduleValues
        |> Dict.foldl
            (\declName declValue ->
                Result.andThen
                    (\aggregate ->
                        if stringStartsWithUpper declName then
                            declValue
                                |> parseTypeDeclarationFromValueTagged
                                |> Result.map
                                    (\typeDeclaration ->
                                        { aggregate
                                            | typeDeclarations =
                                                Dict.insert
                                                    declName
                                                    typeDeclaration
                                                    aggregate.typeDeclarations
                                        }
                                    )

                        else
                            { aggregate
                                | functionDeclarations =
                                    Dict.insert
                                        declName
                                        declValue
                                        aggregate.functionDeclarations
                            }
                                |> Ok
                    )
            )
            (Ok
                { functionDeclarations = Dict.empty
                , typeDeclarations = Dict.empty
                }
            )


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
                            Err ("Failed to decode string: " ++ err)

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
            listItems
                |> List.map
                    (\tagEntry ->
                        case tagEntry of
                            Pine.BlobValue _ ->
                                Err "Is not a list but a blob"

                            Pine.ListValue [ tagNameValue, argumentCountValue ] ->
                                Pine.stringFromValue tagNameValue
                                    |> Result.mapError ((++) "Failed to decode string: ")
                                    |> Result.andThen
                                        (\tagName ->
                                            case Pine.intFromValue argumentCountValue of
                                                Err err ->
                                                    Err ("Failed to decode int: " ++ err)

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
                |> Result.Extra.combine
                |> Result.map (\tags -> { tags = Dict.fromList tags })

        Pine.BlobValue _ ->
            Err "Is not a list but a blob"


parseRecordConstructorFromValue : Pine.Value -> Result String (List String)
parseRecordConstructorFromValue value =
    case value of
        Pine.ListValue listItems ->
            listItems
                |> List.map Pine.stringFromValue
                |> Result.Extra.combine

        Pine.BlobValue _ ->
            Err "Is not a list but a blob"


stringStartsWithUpper : String -> Bool
stringStartsWithUpper string =
    case String.uncons string of
        Nothing ->
            False

        Just ( firstChar, _ ) ->
            Char.isUpper firstChar
