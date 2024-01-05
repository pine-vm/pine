module ElmCompiler exposing (..)

import BigInt
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
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation
import FirCompiler
    exposing
        ( Deconstruction(..)
        , ElmModuleChoiceType
        , ElmModuleInCompilation
        , EmitStack
        , Expression(..)
        , ModuleImports
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
import Json.Encode
import List.Extra
import Pine
import Result.Extra
import Set


type alias ProjectParsedElmFile =
    { projectedModuleName : List String
    , fileText : String
    , parsedModule : Elm.Syntax.File.File
    }


type alias CompilationStack =
    { moduleAliases : Dict.Dict (List String) (List String)
    , availableModules : Dict.Dict (List String) ElmModuleInCompilation
    , availableDeclarations : Dict.Dict String InternalDeclaration
    , elmValuesToExposeToGlobal : Dict.Dict String (List String)
    }


type InternalDeclaration
    = CompiledDeclaration Pine.Value
    | DeconstructionDeclaration Expression


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


pineKernelModuleName : String
pineKernelModuleName =
    "Pine_kernel"


elmStringTypeTagName : String
elmStringTypeTagName =
    "String"


elmRecordTypeTagName : String
elmRecordTypeTagName =
    "Elm_Record"


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
                                            |> List.Extra.find
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
                                                            compileElmModuleTextIntoNamedExports currentAvailableModules moduleToTranslate
                                                                |> Result.mapError
                                                                    ((++)
                                                                        ("Failed to compile elm module '"
                                                                            ++ String.join "." (Elm.Syntax.Node.value (moduleNameFromSyntaxFile moduleToTranslate.parsedModule))
                                                                            ++ "': "
                                                                        )
                                                                    )
                                                                |> Result.map
                                                                    (\( moduleName, moduleValue ) ->
                                                                        Dict.insert moduleName
                                                                            moduleValue
                                                                            aggregate
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


compileElmModuleTextIntoNamedExports :
    Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    -> ProjectParsedElmFile
    -> Result String ( Elm.Syntax.ModuleName.ModuleName, ElmModuleInCompilation )
compileElmModuleTextIntoNamedExports availableModules moduleToTranslate =
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

        declarationsFromChoiceTypes : Dict.Dict String (Dict.Dict String Pine.Value)
        declarationsFromChoiceTypes =
            moduleToTranslate.parsedModule.declarations
                |> List.map Elm.Syntax.Node.value
                |> List.concatMap
                    (\declaration ->
                        case declaration of
                            Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                [ ( Elm.Syntax.Node.value choiceTypeDeclaration.name
                                  , choiceTypeDeclaration.constructors
                                        |> List.map
                                            (Elm.Syntax.Node.value
                                                >> compileElmSyntaxValueConstructor
                                            )
                                        |> Dict.fromList
                                        |> Dict.map
                                            (\name originalDeclaredValue ->
                                                elmDeclarationsOverrides
                                                    |> Dict.get moduleName
                                                    |> Maybe.andThen (Dict.get name)
                                                    |> Maybe.withDefault originalDeclaredValue
                                            )
                                  )
                                ]

                            _ ->
                                []
                    )
                |> Dict.fromList

        declarationsFromTypeAliases : Dict.Dict String Pine.Value
        declarationsFromTypeAliases =
            moduleToTranslate.parsedModule.declarations
                |> List.foldl
                    (\(Elm.Syntax.Node.Node _ declaration) ->
                        case declaration of
                            Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
                                case aliasDeclaration.typeAnnotation of
                                    Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.Record record) ->
                                        Dict.insert
                                            (Elm.Syntax.Node.value aliasDeclaration.name)
                                            (compileElmSyntaxRecordConstructor record)

                                    Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.GenericRecord _ record) ->
                                        Dict.insert
                                            (Elm.Syntax.Node.value aliasDeclaration.name)
                                            (compileElmSyntaxRecordConstructor (Elm.Syntax.Node.value record))

                                    _ ->
                                        identity

                            _ ->
                                identity
                    )
                    Dict.empty

        declarationsFromChoiceTypesTags : Dict.Dict String Pine.Value
        declarationsFromChoiceTypesTags =
            declarationsFromChoiceTypes
                |> Dict.values
                |> List.foldl Dict.union Dict.empty

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

        parsedImports =
            moduleToTranslate.parsedModule.imports
                |> List.map (Elm.Syntax.Node.value >> parseElmSyntaxImport)

        moduleImports =
            moduleImportsFromCompilationStack
                parsedImports
                initialCompilationStack

        initialCompilationStack =
            { moduleAliases = moduleAliases
            , availableModules = availableModules
            , availableDeclarations =
                declarationsFromChoiceTypesTags
                    |> Dict.union declarationsFromTypeAliases
                    |> Dict.map (always CompiledDeclaration)
            , elmValuesToExposeToGlobal =
                elmValuesToExposeToGlobalDefault
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

        initialEmitStack =
            { moduleImports = moduleImports
            , declarationsDependencies = Dict.empty
            , environmentFunctions = []
            , environmentDeconstructions = Dict.empty
            }

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
                        emitModuleDeclarations
                            initialEmitStack
                            { exposedDeclarations =
                                localFunctionDeclarationsCompiled
                                    |> Dict.filter (exposeFunction >> always)
                            , supportingDeclarations =
                                localFunctionDeclarationsCompiled
                            }
                    )

        choiceTypes : Dict.Dict String ElmModuleChoiceType
        choiceTypes =
            declarationsFromChoiceTypes
                |> Dict.map (always (\tags -> { tagsNames = tags |> Dict.keys |> Set.fromList }))
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
                                    |> List.Extra.find (Tuple.first >> (==) function)
                                    |> Maybe.map (Tuple.second >> Tuple.pair name)
                            )
            in
            Ok
                ( moduleName
                , { declarations =
                        [ Dict.toList declarationsFromChoiceTypesTags
                        , Dict.toList declarationsFromTypeAliases
                        , List.filter (Tuple.first >> exposeFunction) functionDeclarations
                        , declarationsValuesForInfix
                        ]
                            |> List.concat
                            |> Dict.fromList
                  , choiceTypes = choiceTypes
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


moduleImportsFromCompilationStack :
    List ModuleImportStatement
    -> CompilationStack
    -> ModuleImports
moduleImportsFromCompilationStack explicitImports compilation =
    let
        importedModulesImplicit =
            compilation.availableModules
                |> Dict.filter (List.member >> (|>) autoImportedModulesNames >> always)

        parsedExplicitImports : List ( List String, ( ElmModuleInCompilation, Dict.Dict String Pine.Value ) )
        parsedExplicitImports =
            explicitImports
                |> List.filterMap
                    (\explicitImport ->
                        compilation.availableModules
                            |> Dict.get explicitImport.canonicalModuleName
                            |> Maybe.map
                                (\availableModule ->
                                    let
                                        exposedDeclarations =
                                            case explicitImport.exposingList of
                                                Nothing ->
                                                    Dict.empty

                                                Just ExposingAll ->
                                                    availableModule.declarations

                                                Just (ExposingSelected exposedNames) ->
                                                    exposedNames
                                                        |> List.concatMap
                                                            (\exposedName ->
                                                                let
                                                                    importedChoiceTypeTags =
                                                                        if not exposedName.open then
                                                                            []

                                                                        else
                                                                            availableModule.choiceTypes
                                                                                |> Dict.get exposedName.name
                                                                                |> Maybe.map .tagsNames
                                                                                |> Maybe.withDefault Set.empty
                                                                                |> Set.toList
                                                                                |> List.filterMap
                                                                                    (\tagName ->
                                                                                        availableModule.declarations
                                                                                            |> Dict.get tagName
                                                                                            |> Maybe.map
                                                                                                (Tuple.pair tagName)
                                                                                    )
                                                                in
                                                                [ availableModule.declarations
                                                                    |> Dict.get exposedName.name
                                                                    |> Maybe.map
                                                                        (Tuple.pair exposedName.name
                                                                            >> List.singleton
                                                                        )
                                                                    |> Maybe.withDefault []
                                                                , importedChoiceTypeTags
                                                                ]
                                                                    |> List.concat
                                                            )
                                                        |> Dict.fromList
                                    in
                                    ( explicitImport.localModuleName
                                    , ( availableModule
                                      , exposedDeclarations
                                      )
                                    )
                                )
                    )

        importedDeclarations =
            [ compilation.elmValuesToExposeToGlobal
                |> Dict.toList
                |> List.filterMap
                    (\( name, moduleName ) ->
                        compilation.availableModules
                            |> Dict.get moduleName
                            |> Maybe.andThen (.declarations >> Dict.get name)
                            |> Maybe.map (Tuple.pair name)
                    )
            , parsedExplicitImports
                |> List.map (Tuple.second >> Tuple.second)
                |> List.map Dict.toList
                |> List.concat
            ]
                |> List.concat
                |> Dict.fromList

        importedModules =
            parsedExplicitImports
                |> List.map (Tuple.mapSecond Tuple.first)
                |> Dict.fromList
                |> Dict.union importedModulesImplicit
    in
    { importedModules = importedModules
    , importedDeclarations = importedDeclarations
    }


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
            case application |> List.map Elm.Syntax.Node.value of
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

        _ ->
            Err
                ("Unsupported type of expression: "
                    ++ (elmExpression |> Elm.Syntax.Expression.encode |> Json.Encode.encode 0)
                )


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
                continueWithNonKernelApplication () =
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
                        continueWithNonKernelApplication ()

                _ ->
                    continueWithNonKernelApplication ()


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
                                                            |> DeconstructionDeclaration
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
                            | availableDeclarations =
                                stackBefore.availableDeclarations
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
        |> Result.map (ListExpression >> List.singleton >> FirCompiler.tagValueExpression elmRecordTypeTagName)


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
compileRecordAccessExpression fieldName =
    List.singleton
        >> FunctionApplicationExpression
            (compileElmSyntaxRecordAccessFunction fieldName)


compileElmSyntaxRecordAccessFunction : String -> Expression
compileElmSyntaxRecordAccessFunction fieldName =
    FunctionExpression
        [ [ ( "record_param", [] ) ] ]
        (PineFunctionApplicationExpression
            (pineExpressionForRecordAccess fieldName Pine.EnvironmentExpression)
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
                List.foldl
                    (\( fieldName, fieldExpr ) aggregate ->
                        PineFunctionApplicationExpression
                            (pineExpressionForRecordUpdate
                                fieldName
                                (listItemFromIndexExpression_Pine 0 Pine.EnvironmentExpression)
                                (listItemFromIndexExpression_Pine 1 Pine.EnvironmentExpression)
                            )
                            (ListExpression
                                [ fieldExpr
                                , aggregate
                                ]
                            )
                    )
                    (ReferenceExpression recordName)
                    settersExpressions
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
                        | availableDeclarations =
                            stackBefore.availableDeclarations
                                |> Dict.union
                                    (Dict.map (always DeconstructionDeclaration)
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
                                            case
                                                autoImportedModulesExposingTagsNames
                                                    |> List.filterMap (Dict.get >> (|>) elmDeclarationsOverrides)
                                                    |> List.foldl Dict.union Dict.empty
                                                    |> Dict.get qualifiedName.name
                                            of
                                                Just tagNameValueFromOverrides ->
                                                    equalCondition
                                                        [ LiteralExpression tagNameValueFromOverrides
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
                                , [ pineExpressionForRecordAccess fieldName Pine.EnvironmentExpression
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

        _ ->
            Err
                ("Unsupported type of pattern: "
                    ++ Json.Encode.encode 0 (Elm.Syntax.Pattern.encode elmPattern)
                )


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


pineExpressionForRecordUpdate : String -> Pine.Expression -> Pine.Expression -> Pine.Expression
pineExpressionForRecordUpdate fieldName fieldNewVal recordExpression =
    let
        recordFieldsExpression =
            pineKernel_ListHead_Pine (listItemFromIndexExpression_Pine 1 recordExpression)
    in
    Pine.ConditionalExpression
        { condition =
            equalCondition_Pine
                [ Pine.LiteralExpression (Pine.valueFromString elmRecordTypeTagName)
                , pineKernel_ListHead_Pine recordExpression
                ]
        , ifTrue =
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString elmRecordTypeTagName)
                , Pine.ListExpression
                    [ pineExpressionRecordUpdateFieldsList fieldName fieldNewVal recordFieldsExpression
                    ]
                ]
        , ifFalse = Pine.ListExpression []
        }


pineExpressionRecordUpdateFieldsList : String -> Pine.Expression -> Pine.Expression -> Pine.Expression
pineExpressionRecordUpdateFieldsList fieldName fieldNewVal recordFieldsExpression =
    let
        expressionEncoded =
            Pine.LiteralExpression buildRecursiveFunctionToUpdateFieldInRecordAsValue
    in
    Pine.DecodeAndEvaluateExpression
        { expression = expressionEncoded
        , environment =
            Pine.ListExpression
                [ expressionEncoded
                , Pine.LiteralExpression (Pine.valueFromString fieldName)
                , fieldNewVal
                , Pine.ListExpression []
                , recordFieldsExpression
                ]
        }


buildRecursiveFunctionToUpdateFieldInRecordAsValue : Pine.Value
buildRecursiveFunctionToUpdateFieldInRecordAsValue =
    Pine.encodeExpressionAsValue buildRecursiveFunctionToUpdateFieldInRecord


{-| Recursively scans through the record fields and replaces every field with the same name as the one to update.
If the field is not found, the original record is returned.
Takes the following arguments:

1.  The function itself, so that we don't have to depend on recursion in the environment.
2.  The name of the field to update.
3.  The new value for the field.
4.  The list of fields that have been processed so far.
5.  The list of fields that are yet to be processed.

-}
buildRecursiveFunctionToUpdateFieldInRecord : Pine.Expression
buildRecursiveFunctionToUpdateFieldInRecord =
    let
        functionReferenceLocalExpression : Pine.Expression
        functionReferenceLocalExpression =
            listItemFromIndexExpression_Pine 0 Pine.EnvironmentExpression

        fieldNameLocalExpression : Pine.Expression
        fieldNameLocalExpression =
            listItemFromIndexExpression_Pine 1 Pine.EnvironmentExpression

        fieldValueLocalExpression : Pine.Expression
        fieldValueLocalExpression =
            listItemFromIndexExpression_Pine 2 Pine.EnvironmentExpression

        processedFieldsLocalExpression : Pine.Expression
        processedFieldsLocalExpression =
            listItemFromIndexExpression_Pine 3 Pine.EnvironmentExpression

        remainingFieldsLocalExpression : Pine.Expression
        remainingFieldsLocalExpression =
            listItemFromIndexExpression_Pine 4 Pine.EnvironmentExpression

        remainingFieldsNextLocalExpression : Pine.Expression
        remainingFieldsNextLocalExpression =
            listItemFromIndexExpression_Pine 0 remainingFieldsLocalExpression
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
                        , fieldNameLocalExpression
                        ]
                , ifTrue =
                    Pine.DecodeAndEvaluateExpression
                        { expression = functionReferenceLocalExpression
                        , environment =
                            Pine.ListExpression
                                [ functionReferenceLocalExpression
                                , fieldNameLocalExpression
                                , fieldValueLocalExpression
                                , Pine.KernelApplicationExpression
                                    { functionName = "concat"
                                    , argument =
                                        Pine.ListExpression
                                            [ processedFieldsLocalExpression
                                            , Pine.ListExpression
                                                [ Pine.ListExpression
                                                    [ fieldNameLocalExpression
                                                    , fieldValueLocalExpression
                                                    ]
                                                ]
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
                                , fieldNameLocalExpression
                                , fieldValueLocalExpression
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


pineExpressionForRecordAccess : String -> Pine.Expression -> Pine.Expression
pineExpressionForRecordAccess fieldName recordExpression =
    let
        recordFieldsExpression =
            pineKernel_ListHead_Pine (listItemFromIndexExpression_Pine 1 recordExpression)
    in
    Pine.ConditionalExpression
        { condition =
            equalCondition_Pine
                [ Pine.LiteralExpression (Pine.valueFromString elmRecordTypeTagName)
                , pineKernel_ListHead_Pine recordExpression
                ]
        , ifTrue = buildRecursiveFunctionToLookupFieldInRecord fieldName recordFieldsExpression
        , ifFalse = Pine.ListExpression []
        }


buildRecursiveFunctionToLookupFieldInRecord : String -> Pine.Expression -> Pine.Expression
buildRecursiveFunctionToLookupFieldInRecord fieldName recordFieldsExpression =
    let
        fieldNameValue =
            Pine.valueFromString fieldName

        remainingFieldsLocalExpression =
            listItemFromIndexExpression_Pine 1 Pine.EnvironmentExpression

        continueWithRemainingExpression =
            Pine.DecodeAndEvaluateExpression
                { expression = listItemFromIndexExpression_Pine 0 Pine.EnvironmentExpression
                , environment =
                    Pine.ListExpression
                        [ listItemFromIndexExpression_Pine 0 Pine.EnvironmentExpression
                        , listSkipExpression_Pine 1 remainingFieldsLocalExpression
                        ]
                }

        recursivePart =
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
                                [ listItemFromIndexExpression_Pine 0 (listItemFromIndexExpression_Pine 0 remainingFieldsLocalExpression)
                                , Pine.LiteralExpression fieldNameValue
                                ]
                        , ifTrue =
                            listItemFromIndexExpression_Pine 1 (listItemFromIndexExpression_Pine 0 remainingFieldsLocalExpression)
                        , ifFalse = continueWithRemainingExpression
                        }
                }

        expressionEncoded =
            Pine.LiteralExpression (Pine.encodeExpressionAsValue recursivePart)
    in
    Pine.DecodeAndEvaluateExpression
        { expression = expressionEncoded
        , environment =
            Pine.ListExpression
                [ expressionEncoded
                , recordFieldsExpression
                ]
        }


compileElmFunctionOrValueLookup : ( List String, String ) -> CompilationStack -> Result String Expression
compileElmFunctionOrValueLookup ( moduleName, localName ) compilation =
    if moduleName == [] then
        case compilation.availableDeclarations |> Dict.get localName of
            Nothing ->
                compileElmFunctionOrValueLookupWithoutLocalResolution ( moduleName, localName ) compilation

            Just (CompiledDeclaration compiledDeclaration) ->
                Ok (compileLookupForInlineableDeclaration ( moduleName, localName ) compiledDeclaration)

            Just (DeconstructionDeclaration deconstruction) ->
                Ok deconstruction

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
    case Dict.get name compilation.elmValuesToExposeToGlobal of
        Nothing ->
            Ok (ReferenceExpression fusedName)

        Just sourceModuleName ->
            getDeclarationValueFromCompilation ( sourceModuleName, name ) compilation
                |> Result.map (compileLookupForInlineableDeclaration ( moduleName, name ))


getDeclarationValueFromCompilation : ( List String, String ) -> CompilationStack -> Result String Pine.Value
getDeclarationValueFromCompilation ( localModuleName, nameInModule ) compilation =
    let
        canonicalModuleName =
            Dict.get localModuleName compilation.moduleAliases
                |> Maybe.withDefault localModuleName

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
                    case Dict.get nameInModule moduleValue.declarations of
                        Nothing ->
                            Err
                                ("Did not find '"
                                    ++ nameInModule
                                    ++ "' in module '"
                                    ++ String.join "." canonicalModuleName
                                    ++ "'. There are "
                                    ++ String.fromInt (Dict.size moduleValue.declarations)
                                    ++ " names available in that module: "
                                    ++ String.join ", " (Dict.keys moduleValue.declarations)
                                )

                        Just declarationValue ->
                            Ok declarationValue
    in
    case Dict.get canonicalModuleName getDeclarationValueFromCompilationOverrides of
        Nothing ->
            continueWithDefault ()

        Just overrides ->
            case Dict.get nameInModule overrides of
                Just overrideValue ->
                    overrideValue

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
                    { moduleImports =
                        { importedModules = Dict.empty
                        , importedDeclarations = Dict.empty
                        }
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
                    { moduleImports =
                        { importedModules = Dict.empty
                        , importedDeclarations = Dict.empty
                        }
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


compileLookupForInlineableDeclaration : ( List String, String ) -> Pine.Value -> Expression
compileLookupForInlineableDeclaration ( moduleName, name ) value =
    let
        fusedName =
            String.join "." (moduleName ++ [ name ])
    in
    if shouldInlineDeclaration name value then
        LiteralExpression value

    else
        ReferenceExpression fusedName


emitModuleValue : ElmModuleInCompilation -> Pine.Value
emitModuleValue parsedModule =
    let
        choiceTypeDescriptions : List ( String, Pine.Value )
        choiceTypeDescriptions =
            parsedModule.choiceTypes
                |> Dict.toList
                |> List.map (Tuple.mapSecond emitChoiceTypeValue)
    in
    (Dict.toList parsedModule.declarations ++ choiceTypeDescriptions)
        |> List.map Pine.valueFromContextExpansionWithName
        |> Pine.ListValue


emitChoiceTypeValue : ElmModuleChoiceType -> Pine.Value
emitChoiceTypeValue choiceType =
    Pine.valueFromContextExpansionWithName
        ( "ChoiceType"
        , choiceType.tagsNames
            |> Set.toList
            |> List.map Pine.valueFromString
            |> Pine.ListValue
        )


emitModuleDeclarations :
    EmitStack
    ->
        { exposedDeclarations : Dict.Dict String Expression
        , supportingDeclarations : Dict.Dict String Expression
        }
    -> Result String (List ( String, Pine.Value ))
emitModuleDeclarations stackBefore declarations =
    declarations.supportingDeclarations
        |> Dict.union declarations.exposedDeclarations
        |> FirCompiler.emitExpressionInDeclarationBlock stackBefore
        |> (\builder ->
                declarations.exposedDeclarations
                    |> Dict.toList
                    |> List.map
                        (\( declarationName, declarationExpression ) ->
                            builder declarationExpression
                                |> Result.andThen evaluateAsIndependentExpression
                                |> Result.mapError ((++) ("Failed for declaration '" ++ declarationName ++ "': "))
                                |> Result.map (Tuple.pair declarationName)
                        )
                    |> Result.Extra.combine
           )


compileElmSyntaxValueConstructor : Elm.Syntax.Type.ValueConstructor -> ( String, Pine.Value )
compileElmSyntaxValueConstructor valueConstructor =
    let
        constructorName =
            Elm.Syntax.Node.value valueConstructor.name
    in
    ( constructorName
    , case List.length valueConstructor.arguments of
        0 ->
            Pine.ListValue
                [ Pine.valueFromString constructorName
                , Pine.ListValue []
                ]

        1 ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString constructorName)
                , Pine.ListExpression [ Pine.EnvironmentExpression ]
                ]
                |> Pine.encodeExpressionAsValue

        2 ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString "List")
                , Pine.ListExpression
                    [ Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "Literal")
                        , Pine.LiteralExpression (Pine.valueFromString constructorName)
                        ]
                    , Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "List")
                        , Pine.ListExpression
                            [ Pine.ListExpression
                                [ Pine.LiteralExpression (Pine.valueFromString "Literal")
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

        argumentsCount ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString constructorName)
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


compileElmSyntaxRecordConstructor : Elm.Syntax.TypeAnnotation.RecordDefinition -> Pine.Value
compileElmSyntaxRecordConstructor record =
    let
        recordFieldNames =
            record |> List.map (Elm.Syntax.Node.value >> Tuple.first >> Elm.Syntax.Node.value)
    in
    FunctionExpression
        (recordFieldNames
            |> List.map (\fieldName -> [ ( fieldName, [] ) ])
        )
        ([ LiteralExpression (Pine.valueFromString elmRecordTypeTagName)
         , [ recordFieldNames
                |> List.map
                    (\fieldName ->
                        [ LiteralExpression (Pine.valueFromString fieldName)
                        , ReferenceExpression fieldName
                        ]
                            |> ListExpression
                    )
                |> ListExpression
           ]
            |> ListExpression
         ]
            |> ListExpression
        )
        |> FirCompiler.emitExpression
            { moduleImports =
                { importedModules = Dict.empty
                , importedDeclarations = Dict.empty
                }
            , declarationsDependencies = Dict.empty
            , environmentFunctions = []
            , environmentDeconstructions = Dict.empty
            }
        |> Result.andThen evaluateAsIndependentExpression
        |> Result.withDefault
            (Pine.valueFromString "Failed to compile record constructor")


shouldInlineDeclaration : String -> Pine.Value -> Bool
shouldInlineDeclaration name value =
    if stringStartsWithUpper name then
        True

    else
        estimatePineValueSize value < 30 * 1000


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
                            |> List.Extra.find
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
valueFromString =
    Pine.valueFromString >> List.singleton >> tagValue elmStringTypeTagName


tagValue : String -> List Pine.Value -> Pine.Value
tagValue tagName tagArguments =
    Pine.ListValue [ Pine.valueFromString tagName, Pine.ListValue tagArguments ]


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
    environmentDeclarations
        |> Dict.filter (stringStartsWithUpper >> always)
        |> Dict.toList
        |> List.map (Tuple.mapFirst (String.split "."))
        |> List.map
            (\( moduleName, moduleValue ) ->
                getDeclarationsFromEnvironment moduleValue
                    |> Result.map (Tuple.pair moduleName)
                    |> Result.mapError ((++) ("Failed to get declarations from module " ++ String.join "." moduleName))
            )
        |> Result.Extra.combine
        |> Result.map Dict.fromList
        |> Result.map
            (\environmentBeforeModules ->
                environmentDeclarations
                    |> Dict.filter (stringStartsWithUpper >> not >> always)
                    |> (\otherDeclarations ->
                            { modules =
                                environmentBeforeModules
                                    |> Dict.map (always separateModuleDeclarations)
                            , otherDeclarations = otherDeclarations
                            }
                       )
            )


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


separateModuleDeclarations : Dict.Dict String Pine.Value -> ElmModuleInCompilation
separateModuleDeclarations moduleValues =
    let
        choiceTypes : Dict.Dict String ElmModuleChoiceType
        choiceTypes =
            moduleValues
                |> Dict.toList
                |> List.filterMap
                    (\( name, value ) ->
                        value
                            |> parseChoiceTypeRecordFromValueTagged
                            |> Result.toMaybe
                            |> Maybe.map (Tuple.pair name)
                    )
                |> Dict.fromList
    in
    { declarations = moduleValues
    , choiceTypes = choiceTypes
    }


parseChoiceTypeRecordFromValueTagged : Pine.Value -> Result String ElmModuleChoiceType
parseChoiceTypeRecordFromValueTagged value =
    case value of
        Pine.BlobValue _ ->
            Err "Is not a list but a blob"

        Pine.ListValue listItems ->
            case listItems of
                [ typeTag, functionRecord ] ->
                    if typeTag == Pine.valueFromString "ChoiceType" then
                        parseChoiceTypeRecordFromValue functionRecord

                    else
                        Err "Is not tagged as 'ChoiceType'"

                _ ->
                    Err
                        ("List does not have the expected number of items: "
                            ++ String.fromInt (List.length listItems)
                        )


parseChoiceTypeRecordFromValue : Pine.Value -> Result String ElmModuleChoiceType
parseChoiceTypeRecordFromValue value =
    case value of
        Pine.ListValue listItems ->
            listItems
                |> List.map Pine.stringFromValue
                |> Result.Extra.combine
                |> Result.map
                    (\tagsNames ->
                        { tagsNames = tagsNames |> Set.fromList }
                    )

        Pine.BlobValue _ ->
            Err "Is not a list but a blob"


stringStartsWithUpper : String -> Bool
stringStartsWithUpper =
    String.uncons >> Maybe.map (Tuple.first >> Char.isUpper) >> Maybe.withDefault False
