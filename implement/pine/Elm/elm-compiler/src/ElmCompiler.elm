module ElmCompiler exposing
    ( CompilationStack
    , ElmModuleChoiceType(..)
    , ElmModuleChoiceTypeTag(..)
    , ElmModuleInCompilation
    , ElmModuleTypeDeclaration(..)
    , InteractiveSubmission(..)
    , ProjectParsedElmFile
    , applicableDeclarationFromConstructorExpression
    , buildExpressionForNewStateAndResponse
    , compilationAndEmitStackFromInteractiveEnvironment
    , compilationAndEmitStackFromModulesInCompilation
    , compileAndEvalParsedInteractiveSubmission
    , compileElmSyntaxExpression
    , compileElmSyntaxFunction
    , compileParsedInteractiveSubmission
    , elmBytesTypeTagNameAsValue
    , elmFloatTypeTagName
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
    , responseExpressionFromString
    , separateEnvironmentDeclarations
    , stringStartsWithUpper
    , valueFromFloat
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
        , pineKernel_Head
        , pineKernel_Head_Pine
        )
import Pine
import Set


type alias ProjectParsedElmFile =
    { fileText : String
    , parsedModule : Elm.Syntax.File.File
    }


type alias CompilationStack =
    { moduleAliases : Dict.Dict (List String) (List String)
    , availableModules : Dict.Dict (List String) ElmModuleInCompilation
    , inlineableDeclarations : List ( String, List Expression -> Expression )
    , localTypeDeclarations : List ( String, ElmModuleTypeDeclaration )
    , exposedDeclarations : List ( String, List (List String) )
    , localAvailableDeclarations : List String
    , depth : Int
    , knownTypes : List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
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


type ModuleImportTopLevelExpose
    = ModuleImportTopLevelExpose
        -- Exposed name
        String
        -- Choice type tags exposed
        Bool


type alias ElmModuleInCompilation =
    { functionDeclarations : List ( String, Pine.Value )
    , typeDeclarations : List ( String, ElmModuleTypeDeclaration )
    }


type ElmModuleTypeDeclaration
    = ElmModuleChoiceTypeDeclaration ElmModuleChoiceType
    | ElmModuleRecordTypeDeclaration (List String)


type ElmModuleChoiceType
    = ElmModuleChoiceType (List ( String, ElmModuleChoiceTypeTag ))


type ElmModuleChoiceTypeTag
    = ElmModuleChoiceTypeTag Int


type InteractiveSubmission
    = ExpressionSubmission Elm.Syntax.Expression.Expression
    | DeclarationSubmission Elm.Syntax.Declaration.Declaration


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


elmBytesTypeTagName : String
elmBytesTypeTagName =
    "Elm_Bytes"


elmBytesTypeTagNameAsValue : Pine.Value
elmBytesTypeTagNameAsValue =
    Pine.valueFromString elmBytesTypeTagName


elmFloatTypeTagName : String
elmFloatTypeTagName =
    "Elm_Float"


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


elmValuesToExposeToGlobalDefault : List ( String, List String )
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
    , ( "toFloat", [ "Basics" ] )
    , ( "floor", [ "Basics" ] )
    , ( "(::)", [ "List" ] )
    , ( "Nothing", [ "Maybe" ] )
    , ( "Just", [ "Maybe" ] )
    , ( "Err", [ "Result" ] )
    , ( "Ok", [ "Result" ] )
    ]


elmDeclarationsOverrides : List ( List String, List ( String, Pine.Value ) )
elmDeclarationsOverrides =
    [ ( [ "Basics" ]
      , [ ( "True"
          , Pine.trueValue
          )
        , ( "False"
          , Pine.falseValue
          )
        ]
      )
    ]


elmDeclarationsOverridesExpressions : List ( String, Expression )
elmDeclarationsOverridesExpressions =
    elmDeclarationsOverrides
        |> List.concatMap
            (\( moduleName, declarations ) ->
                declarations
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
                (List.map (\( file, _ ) -> file) modulesOrderedByDeps)


expandEnvWithModulesOrdered :
    Pine.Value
    -> List ProjectParsedElmFile
    -> Result String { addedModules : List ( List String, Pine.Value ), environment : Pine.Value }
expandEnvWithModulesOrdered environmentBefore newParsedElmModules =
    case getDeclarationsFromEnvironment environmentBefore of
        Err error ->
            Err ("Failed to get declarations from environment: " ++ error)

        Ok ( beforeBatchEnvList, beforeBatchDecls ) ->
            case separateEnvironmentDeclarations beforeBatchDecls of
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
                            []
                    of
                        Err error ->
                            Err error

                        Ok newCompiledModules ->
                            let
                                modulesValues : List ( List String, Pine.Value )
                                modulesValues =
                                    List.foldr
                                        (\( moduleName, moduleStruct ) aggregate ->
                                            let
                                                moduleValue =
                                                    emitModuleValue moduleStruct
                                            in
                                            ( moduleName, moduleValue )
                                                :: aggregate
                                        )
                                        []
                                        newCompiledModules

                                newEnvironmentListEntries =
                                    List.map
                                        (\( moduleName, moduleValue ) ->
                                            Pine.valueFromContextExpansionWithName
                                                ( String.join "." moduleName, moduleValue )
                                        )
                                        modulesValues

                                newEnvironmentList =
                                    List.concat
                                        [ beforeBatchEnvList
                                        , newEnvironmentListEntries
                                        ]
                            in
                            Ok
                                { addedModules = modulesValues
                                , environment = Pine.ListValue newEnvironmentList
                                }


expandEnvWithModulesRecursive :
    Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    -> List ProjectParsedElmFile
    -> List ( Elm.Syntax.ModuleName.ModuleName, ElmModuleInCompilation )
    -> Result String (List ( Elm.Syntax.ModuleName.ModuleName, ElmModuleInCompilation ))
expandEnvWithModulesRecursive beforeBatchModules parsedElmModules compiledModules =
    case parsedElmModules of
        [] ->
            Ok (List.reverse compiledModules)

        moduleToTranslate :: followingModules ->
            let
                (Elm.Syntax.Node.Node _ moduleToTranslateDefinition) =
                    moduleToTranslate.parsedModule.moduleDefinition

                moduleName =
                    Elm.Syntax.Module.moduleName moduleToTranslateDefinition

                availableModules =
                    List.foldl
                        (\( compiledModuleName, compiledModule ) aggregate ->
                            Dict.insert compiledModuleName compiledModule aggregate
                        )
                        beforeBatchModules
                        compiledModules
            in
            case compileElmModuleIntoNamedExports availableModules moduleToTranslate of
                Err error ->
                    Err
                        ("Failed to compile elm module '"
                            ++ String.join "." moduleName
                            ++ "': "
                            ++ error
                        )

                Ok moduleValue ->
                    expandEnvWithModulesRecursive
                        beforeBatchModules
                        followingModules
                        (( moduleName, moduleValue ) :: compiledModules)


compileElmModuleIntoNamedExports :
    Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    -> ProjectParsedElmFile
    -> Result String ElmModuleInCompilation
compileElmModuleIntoNamedExports availableModules moduleToTranslate =
    let
        (Elm.Syntax.Node.Node _ moduleDefSyntax) =
            moduleToTranslate.parsedModule.moduleDefinition

        selfModuleName : Elm.Syntax.ModuleName.ModuleName
        selfModuleName =
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

        localTypeDeclarations : List ( String, ElmModuleTypeDeclaration )
        localTypeDeclarations =
            List.foldl
                (\(Elm.Syntax.Node.Node _ declaration) aggregate ->
                    case declaration of
                        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                            let
                                (Elm.Syntax.Node.Node _ declName) =
                                    choiceTypeDeclaration.name
                            in
                            ( declName
                            , ElmModuleChoiceTypeDeclaration
                                (ElmModuleChoiceType
                                    (List.foldl
                                        (\(Elm.Syntax.Node.Node _ valueConstructor) tagsAggregate ->
                                            let
                                                (Elm.Syntax.Node.Node _ valueConstructorName) =
                                                    valueConstructor.name
                                            in
                                            case Common.assocListGet valueConstructorName elmDeclarationsOverridesExpressions of
                                                Nothing ->
                                                    ( valueConstructorName
                                                    , ElmModuleChoiceTypeTag (List.length valueConstructor.arguments)
                                                    )
                                                        :: tagsAggregate

                                                Just _ ->
                                                    tagsAggregate
                                        )
                                        []
                                        choiceTypeDeclaration.constructors
                                    )
                                )
                            )
                                :: aggregate

                        Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
                            let
                                (Elm.Syntax.Node.Node _ declName) =
                                    aliasDeclaration.name
                            in
                            case aliasDeclaration.typeAnnotation of
                                Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.Record record) ->
                                    ( declName
                                    , ElmModuleRecordTypeDeclaration
                                        (List.map
                                            (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ recordField, _ )) ->
                                                recordField
                                            )
                                            record
                                        )
                                    )
                                        :: aggregate

                                Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.GenericRecord _ (Elm.Syntax.Node.Node _ record)) ->
                                    ( declName
                                    , ElmModuleRecordTypeDeclaration
                                        (List.map
                                            (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ recordField, _ )) ->
                                                recordField
                                            )
                                            record
                                        )
                                    )
                                        :: aggregate

                                _ ->
                                    aggregate

                        _ ->
                            aggregate
                )
                []
                moduleToTranslate.parsedModule.declarations

        ( compilationStackForImport, initialEmitStack ) =
            compilationAndEmitStackFromModulesInCompilation
                availableModules
                { moduleAliases = moduleAliases
                , parsedImports = parsedImports
                , localTypeDeclarations = localTypeDeclarations
                , selfModuleName = selfModuleName
                }

        moduleExposingList : Elm.Syntax.Exposing.Exposing
        moduleExposingList =
            Elm.Syntax.Module.exposingList moduleDefSyntax

        redirectsForInfix : List ( String, String )
        redirectsForInfix =
            List.foldr
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
                            ( "(" ++ operator ++ ")", function ) :: aggregate

                        _ ->
                            aggregate
                )
                []
                moduleToTranslate.parsedModule.declarations

        functionsToExposeForInfix : List String
        functionsToExposeForInfix =
            List.map Tuple.second redirectsForInfix

        localFunctionDeclarations : List ( String, Elm.Syntax.Expression.Function )
        localFunctionDeclarations =
            List.foldr
                (\(Elm.Syntax.Node.Node _ declaration) aggregate ->
                    case declaration of
                        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                            let
                                (Elm.Syntax.Node.Node _ function) =
                                    functionDeclaration.declaration

                                (Elm.Syntax.Node.Node _ name) =
                                    function.name
                            in
                            ( name, functionDeclaration ) :: aggregate

                        _ ->
                            aggregate
                )
                []
                moduleToTranslate.parsedModule.declarations

        localFunctionDeclarationsNames : List String
        localFunctionDeclarationsNames =
            List.map Tuple.first localFunctionDeclarations

        initialCompilationStack : CompilationStack
        initialCompilationStack =
            { availableModules = availableModules
            , moduleAliases = moduleAliases
            , inlineableDeclarations = compilationStackForImport.inlineableDeclarations
            , exposedDeclarations = compilationStackForImport.exposedDeclarations
            , localAvailableDeclarations = localFunctionDeclarationsNames
            , localTypeDeclarations = compilationStackForImport.localTypeDeclarations
            , depth = 0
            , knownTypes = []
            }

        exposedFunctionDecls : List String
        exposedFunctionDecls =
            Common.listUnique
                (List.concat
                    [ functionsToExposeForInfix
                    , case moduleExposingList of
                        Elm.Syntax.Exposing.All _ ->
                            localFunctionDeclarationsNames

                        Elm.Syntax.Exposing.Explicit explicitList ->
                            List.foldl
                                (\(Elm.Syntax.Node.Node _ item) aggregate ->
                                    case item of
                                        Elm.Syntax.Exposing.FunctionExpose name ->
                                            name :: aggregate

                                        _ ->
                                            aggregate
                                )
                                []
                                explicitList
                    ]
                )

        localFunctionsResult : Result String (List ( String, Pine.Value ))
        localFunctionsResult =
            case
                Common.resultListMapCombine
                    (\( functionName, functionDeclaration ) ->
                        case compileElmSyntaxFunction initialCompilationStack functionDeclaration of
                            Err err ->
                                Err ("Failed to compile function '" ++ functionName ++ "': " ++ err)

                            Ok ( _, compiledFunction ) ->
                                Ok ( functionName, compiledFunction )
                    )
                    localFunctionDeclarations
            of
                Err err ->
                    Err err

                Ok localFunctionDeclarationsCompiled ->
                    emitModuleFunctionDeclarations
                        initialEmitStack
                        localFunctionDeclarationsCompiled
                        exposedFunctionDecls
    in
    case localFunctionsResult of
        Err error ->
            Err ("Failed to compile declaration: " ++ error)

        Ok functionDeclarations ->
            let
                declarationsValuesForInfix : List ( String, Pine.Value )
                declarationsValuesForInfix =
                    List.foldr
                        (\( name, function ) aggregate ->
                            case Common.assocListGet function functionDeclarations of
                                Nothing ->
                                    aggregate

                                Just value ->
                                    ( name, value ) :: aggregate
                        )
                        []
                        redirectsForInfix

                exportedFuncsLessInfix : List ( String, Pine.Value )
                exportedFuncsLessInfix =
                    List.foldl
                        (\( declName, declValue ) aggregate ->
                            if List.member declName exposedFunctionDecls then
                                ( declName, declValue ) :: aggregate

                            else
                                aggregate
                        )
                        []
                        functionDeclarations
            in
            Ok
                { functionDeclarations =
                    List.concat
                        [ exportedFuncsLessInfix
                        , declarationsValuesForInfix
                        ]
                , typeDeclarations = localTypeDeclarations
                }


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
                    ModuleImportTopLevelExpose infixExpose False

                Elm.Syntax.Exposing.FunctionExpose functionExpose ->
                    ModuleImportTopLevelExpose functionExpose False

                Elm.Syntax.Exposing.TypeOrAliasExpose typeOrAlias ->
                    ModuleImportTopLevelExpose typeOrAlias False

                Elm.Syntax.Exposing.TypeExpose typeExpose ->
                    ModuleImportTopLevelExpose
                        typeExpose.name
                        (case typeExpose.open of
                            Nothing ->
                                False

                            _ ->
                                True
                        )

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
        , localTypeDeclarations : List ( String, ElmModuleTypeDeclaration )
        , selfModuleName : List String
        }
    -> ( CompilationStack, EmitStack )
compilationAndEmitStackFromModulesInCompilation availableModules { moduleAliases, parsedImports, localTypeDeclarations, selfModuleName } =
    let
        exposedDeclarationsViaImports : Dict.Dict String (List (List String))
        exposedDeclarationsViaImports =
            exposedDeclarationsFromImportStatements
                parsedImports
                availableModules

        exposedDeclarationsFromAutoImport : List ( String, List (List String) )
        exposedDeclarationsFromAutoImport =
            List.foldl
                (\( declName, coreModuleName ) aggregate ->
                    if coreModuleName == selfModuleName then
                        aggregate

                    else
                        ( declName, [ coreModuleName ] ) :: aggregate
                )
                []
                elmValuesToExposeToGlobalDefault

        exposedDeclarations : List ( String, List (List String) )
        exposedDeclarations =
            List.concat
                [ Dict.toList exposedDeclarationsViaImports
                , exposedDeclarationsFromAutoImport
                ]

        compilationStack : CompilationStack
        compilationStack =
            { availableModules = availableModules
            , moduleAliases = moduleAliases
            , inlineableDeclarations = []
            , localTypeDeclarations = localTypeDeclarations
            , exposedDeclarations = exposedDeclarations
            , localAvailableDeclarations = []
            , depth = 0
            , knownTypes = []
            }

        importedFunctionsBeforeParse : List ( List String, List ( String, Pine.Value ) )
        importedFunctionsBeforeParse =
            List.map
                (\( moduleName, availableModule ) ->
                    ( moduleName, availableModule.functionDeclarations )
                )
                (Dict.toList availableModules)

        importedFunctions : List ( List String, List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) ) )
        importedFunctions =
            List.map
                (Tuple.mapSecond
                    (List.map
                        (\( declName, functionValue ) ->
                            let
                                ( paramCount, expectedEnv ) =
                                    case FirCompiler.parseFunctionRecordFromValueTagged functionValue of
                                        Err _ ->
                                            ( 0
                                            , FirCompiler.IndependentEnvironment
                                            )

                                        Ok (FirCompiler.ParsedFunctionValue _ _ parameterCount _ _) ->
                                            ( parameterCount
                                            , FirCompiler.ImportedEnvironment []
                                            )
                            in
                            ( declName
                            , ( FirCompiler.EnvironmentFunctionEntry paramCount expectedEnv
                              , functionValue
                              )
                            )
                        )
                    )
                )
                importedFunctionsBeforeParse

        emitStack : FirCompiler.EmitStack
        emitStack =
            { importedFunctions = importedFunctions
            , importedFunctionsToInline = []
            , environmentFunctions = []
            , environmentDeconstructions = []
            }
    in
    ( compilationStack
    , emitStack
    )


exposedDeclarationsFromImportStatements :
    List ModuleImportStatement
    -> Dict.Dict (List String) ElmModuleInCompilation
    -> Dict.Dict String (List (List String))
exposedDeclarationsFromImportStatements explicitImports availableModules =
    List.foldl
        (\explicitImport aggregate ->
            case Dict.get explicitImport.canonicalModuleName availableModules of
                Nothing ->
                    aggregate

                Just availableModule ->
                    let
                        moduleExposedNames : List String
                        moduleExposedNames =
                            case explicitImport.exposingList of
                                Nothing ->
                                    []

                                Just ExposingAll ->
                                    List.concat
                                        [ List.map Tuple.first availableModule.functionDeclarations
                                        , List.concatMap
                                            (\( typeName, typeDecl ) ->
                                                let
                                                    choiceTypeTagNames =
                                                        case typeDecl of
                                                            ElmModuleChoiceTypeDeclaration (ElmModuleChoiceType tags) ->
                                                                List.map Tuple.first tags

                                                            ElmModuleRecordTypeDeclaration _ ->
                                                                []
                                                in
                                                typeName :: choiceTypeTagNames
                                            )
                                            availableModule.typeDeclarations
                                        ]

                                Just (ExposingSelected topLevels) ->
                                    List.concatMap
                                        (\(ModuleImportTopLevelExpose topLevelName exposeTags) ->
                                            let
                                                choiceTypeTagNames =
                                                    if exposeTags then
                                                        case Common.assocListGet topLevelName availableModule.typeDeclarations of
                                                            Nothing ->
                                                                []

                                                            Just (ElmModuleChoiceTypeDeclaration (ElmModuleChoiceType tags)) ->
                                                                List.map Tuple.first tags

                                                            Just (ElmModuleRecordTypeDeclaration _) ->
                                                                []

                                                    else
                                                        []

                                                topLevelNameMapped =
                                                    {-
                                                       2024-09-21:
                                                       Looks like stil4m/elm-syntax v 7.3.2 does not include the parens for entries exposing an operator.
                                                       Therefore we add parens here for these cases so that lookups work.
                                                    -}
                                                    if nameLooksLikeOperator topLevelName then
                                                        if not (String.startsWith "(" topLevelName) then
                                                            "(" ++ topLevelName ++ ")"

                                                        else
                                                            topLevelName

                                                    else
                                                        topLevelName
                                            in
                                            topLevelNameMapped :: choiceTypeTagNames
                                        )
                                        topLevels
                    in
                    List.foldl
                        (\exposedName dict ->
                            let
                                moduleNames =
                                    case Dict.get exposedName dict of
                                        Nothing ->
                                            [ explicitImport.canonicalModuleName ]

                                        Just moduleNamesBefore ->
                                            if List.member explicitImport.canonicalModuleName moduleNamesBefore then
                                                moduleNamesBefore

                                            else
                                                explicitImport.canonicalModuleName :: moduleNamesBefore
                            in
                            Dict.insert
                                exposedName
                                moduleNames
                                dict
                        )
                        aggregate
                        moduleExposedNames
        )
        Dict.empty
        explicitImports


nameLooksLikeOperator : String -> Bool
nameLooksLikeOperator declName =
    String.any charIndicatesOperator declName


charIndicatesOperator : Char -> Bool
charIndicatesOperator char =
    case char of
        '(' ->
            True

        ')' ->
            True

        '.' ->
            True

        '=' ->
            True

        '|' ->
            True

        '&' ->
            True

        '<' ->
            True

        '>' ->
            True

        '+' ->
            True

        '-' ->
            True

        '*' ->
            True

        '/' ->
            True

        '?' ->
            True

        '^' ->
            True

        _ ->
            False


mapTypeDeclarationForImport : Bool -> ElmModuleTypeDeclaration -> ElmModuleTypeDeclaration
mapTypeDeclarationForImport open typeDeclaration =
    case typeDeclaration of
        ElmModuleRecordTypeDeclaration _ ->
            typeDeclaration

        ElmModuleChoiceTypeDeclaration _ ->
            if open then
                typeDeclaration

            else
                ElmModuleChoiceTypeDeclaration (ElmModuleChoiceType [])


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
            case negatedElmExpression of
                Elm.Syntax.Expression.Floatable negatedFloat ->
                    Ok (LiteralExpression (valueFromFloat -negatedFloat))

                Elm.Syntax.Expression.Integer negatedInteger ->
                    Ok (LiteralExpression (Pine.valueFromInt -negatedInteger))

                _ ->
                    case compileElmSyntaxExpression stack negatedElmExpression of
                        Err error ->
                            Err ("Failed to compile negated expression: " ++ error)

                        Ok negatedExpression ->
                            Ok
                                (KernelApplicationExpression
                                    "negate"
                                    negatedExpression
                                )

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            compileElmFunctionOrValueLookup ( moduleName, localName ) stack

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    Err "Invalid shape of application: Zero elements in the list"

                (Elm.Syntax.Node.Node _ appliedFunctionElmSyntax) :: argumentsElmSyntax ->
                    compileElmSyntaxApplication
                        stack
                        appliedFunctionElmSyntax
                        (List.map Elm.Syntax.Node.value argumentsElmSyntax)

        Elm.Syntax.Expression.OperatorApplication operator _ (Elm.Syntax.Node.Node _ leftExpr) (Elm.Syntax.Node.Node _ rightExpr) ->
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
                                    Ok (ConditionalExpression conditionExpression expressionIfFalse expressionIfTrue)

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

        Elm.Syntax.Expression.Floatable float ->
            Ok (LiteralExpression (valueFromFloat float))

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "Unsupported type of expression: GLSLExpression"

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
                Elm.Syntax.Expression.FunctionOrValue moduleNameBeforeImport declName ->
                    case sourceModuleNameFromImports ( moduleNameBeforeImport, declName ) stack of
                        Err err ->
                            Err err

                        Ok moduleName ->
                            let
                                continueWithDefaultNamedApplication () =
                                    case tryApplyNamedInlineableOrTypeDecl moduleName declName arguments stack of
                                        Just fromTypeDecl ->
                                            Ok fromTypeDecl

                                        Nothing ->
                                            if stringStartsWithUpper declName then
                                                Err
                                                    ("Did not find resolution for type reference: "
                                                        ++ String.join "." (List.concat [ moduleName, [ declName ] ])
                                                    )

                                            else
                                                continueWithDefaultApplication ()
                            in
                            case moduleName of
                                [ "Pine_kernel" ] ->
                                    case arguments of
                                        [ singleArgumentExpression ] ->
                                            Ok
                                                (KernelApplicationExpression
                                                    declName
                                                    singleArgumentExpression
                                                )

                                        _ ->
                                            Err "Invalid argument list for kernel application: Wrap arguments into a single list expression"

                                [ "Debug" ] ->
                                    case declName of
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


tryApplyNamedInlineableOrTypeDecl :
    List String
    -> String
    -> List Expression
    -> CompilationStack
    -> Maybe Expression
tryApplyNamedInlineableOrTypeDecl moduleName declName arguments stack =
    case Common.assocListGet declName elmDeclarationsOverridesExpressions of
        Just declarationOverride ->
            Just declarationOverride

        Nothing ->
            case Dict.get moduleName stack.availableModules of
                Nothing ->
                    case Common.assocListGet declName stack.inlineableDeclarations of
                        Just applicableDeclaration ->
                            Just (applicableDeclaration arguments)

                        _ ->
                            if stringStartsWithUpper declName then
                                tryApplyNamedTypeDeclaration declName arguments stack.localTypeDeclarations

                            else
                                Nothing

                Just moduleInCompilation ->
                    if stringStartsWithUpper declName then
                        tryApplyNamedTypeDeclaration declName arguments moduleInCompilation.typeDeclarations

                    else
                        Nothing


tryApplyNamedTypeDeclaration :
    String
    -> List Expression
    -> List ( String, ElmModuleTypeDeclaration )
    -> Maybe Expression
tryApplyNamedTypeDeclaration declName arguments typeDeclarations =
    case Common.assocListGet declName typeDeclarations of
        Just (ElmModuleRecordTypeDeclaration fields) ->
            Just (compileElmRecordConstructor fields arguments)

        _ ->
            let
                allTags : List ( String, ElmModuleChoiceTypeTag )
                allTags =
                    List.concatMap
                        (\( _, typeDecl ) ->
                            case typeDecl of
                                ElmModuleChoiceTypeDeclaration (ElmModuleChoiceType tags) ->
                                    tags

                                _ ->
                                    []
                        )
                        typeDeclarations
            in
            case Common.assocListGet declName allTags of
                Just (ElmModuleChoiceTypeTag paramCount) ->
                    Just (compileElmChoiceTypeTagConstructor ( declName, paramCount ) arguments)

                _ ->
                    Nothing


compileElmSyntaxLetBlock :
    CompilationStack
    -> Elm.Syntax.Expression.LetBlock
    -> Result String Expression
compileElmSyntaxLetBlock stackBefore letBlock =
    case
        Common.resultListMapCombine
            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                case letDeclaration of
                    Elm.Syntax.Expression.LetFunction letFunction ->
                        case letFunction.signature of
                            Nothing ->
                                Ok []

                            Just (Elm.Syntax.Node.Node _ signature) ->
                                let
                                    (Elm.Syntax.Node.Node _ letFunctionDecl) =
                                        letFunction.declaration

                                    (Elm.Syntax.Node.Node _ declName) =
                                        letFunctionDecl.name

                                    (Elm.Syntax.Node.Node _ typeAnnotation) =
                                        signature.typeAnnotation
                                in
                                Ok
                                    [ ( declName
                                      , ( Nothing
                                        , Just typeAnnotation
                                        )
                                      )
                                    ]

                    Elm.Syntax.Expression.LetDestructuring (Elm.Syntax.Node.Node _ pattern) (Elm.Syntax.Node.Node _ destructuredExpressionElm) ->
                        case compileElmSyntaxExpression stackBefore destructuredExpressionElm of
                            Err err ->
                                Err err

                            Ok destructuredExpression ->
                                case compileElmSyntaxPattern stackBefore pattern of
                                    Err err ->
                                        Err err

                                    Ok (CompiledElmSyntaxPattern _ _ patternDecls) ->
                                        Ok
                                            (List.map
                                                (\( declName, deconsExpr ) ->
                                                    ( declName
                                                    , ( Just
                                                            (applicableDeclarationFromConstructorExpression
                                                                (expressionForDeconstructions
                                                                    deconsExpr
                                                                    destructuredExpression
                                                                )
                                                            )
                                                      , Nothing
                                                      )
                                                    )
                                                )
                                                patternDecls
                                            )
            )
            letBlock.declarations
    of
        Err err ->
            Err err

        Ok newAvailableDeclarations ->
            let
                newInlineableDecls : List ( String, List Expression -> Expression )
                newInlineableDecls =
                    List.concatMap
                        (List.filterMap
                            (\( declName, ( maybeInline, _ ) ) ->
                                case maybeInline of
                                    Nothing ->
                                        Nothing

                                    Just inlineFunc ->
                                        Just ( declName, inlineFunc )
                            )
                        )
                        newAvailableDeclarations

                newTypeAnnotations : List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
                newTypeAnnotations =
                    List.concatMap
                        (List.filterMap
                            (\( declName, ( _, maybeTypeAnnotation ) ) ->
                                case maybeTypeAnnotation of
                                    Nothing ->
                                        Nothing

                                    Just typeAnnotation ->
                                        Just ( declName, typeAnnotation )
                            )
                        )
                        newAvailableDeclarations

                inlineableDeclarations =
                    List.concat
                        [ newInlineableDecls
                        , stackBefore.inlineableDeclarations
                        ]

                stack =
                    { stackBefore
                        | inlineableDeclarations = inlineableDeclarations
                        , localAvailableDeclarations =
                            List.concat
                                [ List.concatMap (List.map Tuple.first) newAvailableDeclarations
                                , stackBefore.localAvailableDeclarations
                                ]
                        , knownTypes =
                            List.concat
                                [ newTypeAnnotations
                                , stackBefore.knownTypes
                                ]
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
                                (inlineDeclBlockIfSimple
                                    (List.concat letEntries)
                                    expression
                                )


inlineDeclBlockIfSimple : List ( String, Expression ) -> Expression -> Expression
inlineDeclBlockIfSimple blockDecls expression =
    let
        aggregateRefs : List String
        aggregateRefs =
            List.concatMap
                (\declExpr ->
                    FirCompiler.listUnboundReferencesInExpression declExpr []
                )
                (expression :: List.map Tuple.second blockDecls)

        refsBlockDeclMoreThanOnce : Bool
        refsBlockDeclMoreThanOnce =
            List.any
                (\( declName, _ ) ->
                    let
                        declNameString : String
                        declNameString =
                            declName

                        appearanceCount : Int
                        appearanceCount =
                            Common.listCount
                                (\ref -> ref == declNameString)
                                aggregateRefs
                    in
                    appearanceCount > 1
                )
                blockDecls
    in
    {-
       There is no need to check for recursive references in the block declarations separately,
       because no recursive references can be reachable is the reference count is less than 2.
    -}
    if refsBlockDeclMoreThanOnce then
        DeclarationBlockExpression
            blockDecls
            expression

    else
        FirCompiler.inlineLocalReferences
            blockDecls
            expression


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

                        Ok (CompiledElmSyntaxPattern _ _ patternDecls) ->
                            Ok
                                (List.map
                                    (\( declName, deconsExpr ) ->
                                        ( declName
                                        , expressionForDeconstructions deconsExpr compiledExpression
                                        )
                                    )
                                    patternDecls
                                )


compileElmSyntaxFunction :
    CompilationStack
    -> Elm.Syntax.Expression.Function
    -> Result String ( String, Expression )
compileElmSyntaxFunction stackBefore function =
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

        newKnownTypes : List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
        newKnownTypes =
            case function.signature of
                Nothing ->
                    []

                Just (Elm.Syntax.Node.Node _ signature) ->
                    let
                        (Elm.Syntax.Node.Node _ typeAnnotation) =
                            signature.typeAnnotation
                    in
                    collectTypesFromFunctionTypeAnnotation typeAnnotation functionDeclaration.arguments

        stack =
            { moduleAliases = stackBefore.moduleAliases
            , availableModules = stackBefore.availableModules
            , inlineableDeclarations = stackBefore.inlineableDeclarations
            , exposedDeclarations = stackBefore.exposedDeclarations
            , localAvailableDeclarations = stackBefore.localAvailableDeclarations
            , localTypeDeclarations = stackBefore.localTypeDeclarations
            , depth = stackBefore.depth
            , knownTypes = List.concat [ newKnownTypes, stackBefore.knownTypes ]
            }
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


collectTypesFromFunctionTypeAnnotation :
    Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
collectTypesFromFunctionTypeAnnotation typeAnnotation params =
    case typeAnnotation of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation (Elm.Syntax.Node.Node _ typeAnnotationParam) (Elm.Syntax.Node.Node _ typeAnnotationAfter) ->
            case params of
                [] ->
                    []

                (Elm.Syntax.Node.Node _ param) :: restParams ->
                    let
                        currentNamesTypes : List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
                        currentNamesTypes =
                            case param of
                                Elm.Syntax.Pattern.VarPattern name ->
                                    [ ( name, typeAnnotationParam ) ]

                                _ ->
                                    []
                    in
                    List.concat
                        [ currentNamesTypes
                        , collectTypesFromFunctionTypeAnnotation typeAnnotationAfter restParams
                        ]

        _ ->
            []


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

                    Ok (CompiledElmSyntaxPattern _ _ patternDecls) ->
                        Ok patternDecls
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
                        Err ("Failed to compile record field '" ++ fieldName ++ "': " ++ error)

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
            (ReferenceExpression [] "record-param")
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
                        [ ReferenceExpression [] recordName
                        , ListExpression
                            (List.map
                                (\( fieldName, fieldExpr ) ->
                                    ListExpression
                                        [ LiteralExpression (Pine.valueFromString fieldName)
                                        , fieldExpr
                                        ]
                                )
                                (List.sortBy (\( fieldName, _ ) -> fieldName) settersExpressions)
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
            let
                switchedExprFuncApps : List ( Expression, List Expression )
                switchedExprFuncApps =
                    FirCompiler.listFunctionAppExpressions expression
            in
            case switchedExprFuncApps of
                [] ->
                    case compileCaseBlockInline stack expression caseBlock.cases of
                        Err err ->
                            Err err

                        Ok inlineVariant ->
                            Ok inlineVariant

                _ ->
                    let
                        pseudoParamName =
                            {-
                               Adapt to current limitation in FirCompiler:
                               Since FirCompiler does not yet support shadowing, ensure we create a unique name here, by appending stack depth.
                            -}
                            "case-expr-" ++ String.fromInt stack.depth

                        innerExpr : Expression
                        innerExpr =
                            FirCompiler.ReferenceExpression [] pseudoParamName
                    in
                    case compileCaseBlockInline stack innerExpr caseBlock.cases of
                        Err err ->
                            Err err

                        Ok casesFunction ->
                            Ok
                                (FunctionApplicationExpression
                                    (FunctionExpression
                                        [ [ ( pseudoParamName, [] ) ] ]
                                        casesFunction
                                    )
                                    [ expression ]
                                )


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
                    case deconstructedCase.conditionExpressions of
                        [] ->
                            deconstructedCase.thenExpression

                        firstCondition :: otherConditions ->
                            ConditionalExpression
                                (List.foldl
                                    (\conditionExpression nextConditionExpression ->
                                        ConditionalExpression
                                            conditionExpression
                                            (LiteralExpression Pine.falseValue)
                                            nextConditionExpression
                                    )
                                    firstCondition
                                    otherConditions
                                )
                                nextBlockExpression
                                deconstructedCase.thenExpression
            in
            Ok
                (List.foldr
                    conditionalFromCase
                    (PineFunctionApplicationExpression
                        -- Crash in case none of the branches match.
                        (Pine.ParseAndEvalExpression
                            (Pine.LiteralExpression stringAsValue_errorNoMatchingBranch)
                            Pine.EnvironmentExpression
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

        Ok (CompiledElmSyntaxPattern deconsConditions deconsCannotMatchEmpty deconsDecls) ->
            let
                deconstructionDeclarations : List ( String, Expression )
                deconstructionDeclarations =
                    List.foldl
                        (\( declName, deconsExpr ) aggregate ->
                            ( declName
                            , expressionForDeconstructions deconsExpr caseBlockValueExpression
                            )
                                :: aggregate
                        )
                        []
                        deconsDecls

                inlineableDeclarations : List ( String, List Expression -> Expression )
                inlineableDeclarations =
                    List.concat
                        [ List.map
                            (\( declName, declExpr ) ->
                                ( declName
                                , applicableDeclarationFromConstructorExpression declExpr
                                )
                            )
                            deconstructionDeclarations
                        , stackBefore.inlineableDeclarations
                        ]

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
                            deconsConditions caseBlockValueExpression
                        , thenExpression =
                            case deconsDecls of
                                [] ->
                                    expression

                                _ ->
                                    DeclarationBlockExpression
                                        deconstructionDeclarations
                                        expression
                        }


type CompiledElmSyntaxPattern
    = CompiledElmSyntaxPattern
        -- Condition expressions
        (Expression -> List Expression)
        -- Cannot match empty
        Bool
        -- Declarations
        (List ( String, List Deconstruction ))


compileElmSyntaxPattern :
    CompilationStack
    -> Elm.Syntax.Pattern.Pattern
    -> Result String CompiledElmSyntaxPattern
compileElmSyntaxPattern compilation elmPattern =
    case elmPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok
                (CompiledElmSyntaxPattern
                    (always [])
                    False
                    []
                )

        Elm.Syntax.Pattern.ListPattern listElements ->
            compileElmSyntaxPatternListOrTupleItems
                compilation
                listElements
                True

        Elm.Syntax.Pattern.TuplePattern tupleElements ->
            compileElmSyntaxPatternListOrTupleItems
                compilation
                tupleElements
                False

        Elm.Syntax.Pattern.UnConsPattern (Elm.Syntax.Node.Node _ unconsLeft) (Elm.Syntax.Node.Node _ unconsRight) ->
            {-
               TODO: Check optimizations possible via consolidation of nested uncons patterns.
            -}
            case compileElmSyntaxPattern compilation unconsLeft of
                Err err ->
                    Err err

                Ok (CompiledElmSyntaxPattern leftSideConditions leftSideCannotMatchEmpty leftSideDecls) ->
                    case compileElmSyntaxPattern compilation unconsRight of
                        Err err ->
                            Err err

                        Ok (CompiledElmSyntaxPattern rightSideConditions rightSideCannotMatchEmpty rightSideDecls) ->
                            let
                                conditionExpressions : Expression -> List Expression
                                conditionExpressions deconstructedExpression =
                                    let
                                        explicitLengthConditions : List Expression
                                        explicitLengthConditions =
                                            if leftSideCannotMatchEmpty then
                                                []

                                            else if rightSideCannotMatchEmpty then
                                                []

                                            else
                                                [ KernelApplicationExpression
                                                    "negate"
                                                    (equalCondition
                                                        [ KernelApplicationExpression
                                                            "length"
                                                            deconstructedExpression
                                                        , LiteralExpression (Pine.valueFromInt 0)
                                                        ]
                                                    )
                                                ]
                                    in
                                    List.concat
                                        [ explicitLengthConditions
                                        , leftSideConditions
                                            (listItemFromIndexExpression 0 deconstructedExpression)
                                        , rightSideConditions
                                            (listSkipExpression 1 deconstructedExpression)
                                        ]

                                declarations : List ( String, List Deconstruction )
                                declarations =
                                    List.concat
                                        [ List.map
                                            (\( declName, deconstruction ) ->
                                                ( declName, ListItemDeconstruction 0 :: deconstruction )
                                            )
                                            leftSideDecls
                                        , List.map
                                            (\( declName, deconstruction ) ->
                                                ( declName, SkipItemsDeconstruction 1 :: deconstruction )
                                            )
                                            rightSideDecls
                                        ]
                            in
                            Ok
                                (CompiledElmSyntaxPattern
                                    conditionExpressions
                                    True
                                    declarations
                                )

        Elm.Syntax.Pattern.NamedPattern qualifiedName choiceTypeArgumentPatterns ->
            case sourceModuleNameFromImports ( qualifiedName.moduleName, qualifiedName.name ) compilation of
                Err err ->
                    Err err

                Ok moduleName ->
                    case
                        Common.resultListIndexedMapCombine
                            (\( argIndex, Elm.Syntax.Node.Node _ argPattern ) ->
                                case compileItemPatternForIndex compilation argIndex argPattern of
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
                                typeInfoMaybe : Maybe ( String, ElmModuleChoiceType )
                                typeInfoMaybe =
                                    case moduleName of
                                        [] ->
                                            {-
                                               TODO: Expand lookup of type to also support cases of import all (exposing (..))
                                            -}
                                            case Common.assocListGet qualifiedName.name compilation.localTypeDeclarations of
                                                Nothing ->
                                                    Nothing

                                                Just typeDeclaration ->
                                                    case typeDeclaration of
                                                        ElmModuleChoiceTypeDeclaration choiceTypeDeclaration ->
                                                            Just ( qualifiedName.name, choiceTypeDeclaration )

                                                        _ ->
                                                            Nothing

                                        sourceModuleName ->
                                            case Dict.get sourceModuleName compilation.availableModules of
                                                Nothing ->
                                                    Nothing

                                                Just moduleInCompilation ->
                                                    Common.listMapFind
                                                        (\( typeName, typeDeclaration ) ->
                                                            case typeDeclaration of
                                                                ElmModuleChoiceTypeDeclaration (ElmModuleChoiceType choiceTypeTags) ->
                                                                    case Common.assocListGet qualifiedName.name choiceTypeTags of
                                                                        Nothing ->
                                                                            Nothing

                                                                        Just _ ->
                                                                            Just ( typeName, ElmModuleChoiceType choiceTypeTags )

                                                                _ ->
                                                                    Nothing
                                                        )
                                                        moduleInCompilation.typeDeclarations

                                tagIsOnlyPossible : Bool
                                tagIsOnlyPossible =
                                    case typeInfoMaybe of
                                        Nothing ->
                                            False

                                        Just ( _, ElmModuleChoiceType choiceTypeTags ) ->
                                            case List.length choiceTypeTags of
                                                1 ->
                                                    True

                                                _ ->
                                                    False

                                conditionExpressions : Expression -> List Expression
                                conditionExpressions deconstructedExpression =
                                    let
                                        matchingTagConditions =
                                            if tagIsOnlyPossible then
                                                []

                                            else
                                                [ case Common.assocListGet qualifiedName.name elmDeclarationsOverridesExpressions of
                                                    Just tagNameExpressionFromOverrides ->
                                                        equalCondition
                                                            [ tagNameExpressionFromOverrides
                                                            , deconstructedExpression
                                                            ]

                                                    Nothing ->
                                                        equalCondition
                                                            [ LiteralExpression (Pine.valueFromString qualifiedName.name)
                                                            , pineKernel_Head deconstructedExpression
                                                            ]
                                                ]

                                        argumentsConditions =
                                            List.concatMap
                                                (\(CompiledElmSyntaxPattern conditions _ _) ->
                                                    conditions
                                                        (listItemFromIndexExpression 1 deconstructedExpression)
                                                )
                                                itemsResults
                                    in
                                    List.concat [ matchingTagConditions, argumentsConditions ]

                                mergedDeclarations : List ( String, List Deconstruction )
                                mergedDeclarations =
                                    List.concatMap
                                        (\(CompiledElmSyntaxPattern _ _ declarations) ->
                                            List.map
                                                (\( declName, deconstruction ) ->
                                                    ( declName, ListItemDeconstruction 1 :: deconstruction )
                                                )
                                                declarations
                                        )
                                        itemsResults

                                cannotMatchEmpty : Bool
                                cannotMatchEmpty =
                                    if tagIsOnlyPossible then
                                        List.any
                                            (\(CompiledElmSyntaxPattern _ itemCannotMatchEmpty _) ->
                                                itemCannotMatchEmpty
                                            )
                                            itemsResults

                                    else
                                        True
                            in
                            Ok
                                (CompiledElmSyntaxPattern
                                    conditionExpressions
                                    cannotMatchEmpty
                                    mergedDeclarations
                                )

        Elm.Syntax.Pattern.CharPattern char ->
            compilePatternOnlyEqualsCondition (LiteralExpression (Pine.valueFromChar char))

        Elm.Syntax.Pattern.IntPattern int ->
            compilePatternOnlyEqualsCondition (LiteralExpression (Pine.valueFromInt int))

        Elm.Syntax.Pattern.HexPattern int ->
            compilePatternOnlyEqualsCondition (LiteralExpression (Pine.valueFromInt int))

        Elm.Syntax.Pattern.StringPattern string ->
            compilePatternOnlyEqualsCondition (LiteralExpression (valueFromString string))

        Elm.Syntax.Pattern.VarPattern name ->
            Ok
                (CompiledElmSyntaxPattern
                    (always [])
                    False
                    [ ( name
                      , []
                      )
                    ]
                )

        Elm.Syntax.Pattern.RecordPattern fieldsElements ->
            Ok
                (CompiledElmSyntaxPattern
                    (always [])
                    -- Elm records are internally represented as tagged values.
                    True
                    (List.map
                        (\(Elm.Syntax.Node.Node _ fieldName) ->
                            ( fieldName
                            , [ PineFunctionApplicationDeconstruction
                                    (Pine.ParseAndEvalExpression
                                        (Pine.LiteralExpression pineFunctionForRecordAccessAsValue)
                                        (Pine.ListExpression
                                            [ Pine.environmentExpr
                                            , Pine.LiteralExpression (Pine.valueFromString fieldName)
                                            ]
                                        )
                                    )
                              ]
                            )
                        )
                        fieldsElements
                    )
                )

        Elm.Syntax.Pattern.AsPattern (Elm.Syntax.Node.Node _ aliasedPattern) (Elm.Syntax.Node.Node _ alias) ->
            case compileElmSyntaxPattern compilation aliasedPattern of
                Err err ->
                    Err err

                Ok (CompiledElmSyntaxPattern asCompiledConditions asCompiledCannotMatchEmpty asCompiledDecls) ->
                    Ok
                        (CompiledElmSyntaxPattern
                            asCompiledConditions
                            asCompiledCannotMatchEmpty
                            (( alias, [] ) :: asCompiledDecls)
                        )

        Elm.Syntax.Pattern.ParenthesizedPattern (Elm.Syntax.Node.Node _ parenthesized) ->
            compileElmSyntaxPattern compilation parenthesized

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                (CompiledElmSyntaxPattern
                    (always [])
                    False
                    []
                )

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "Unsupported type of pattern: FloatPattern"


compileElmSyntaxPatternListOrTupleItems :
    CompilationStack
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> Bool
    -> Result String CompiledElmSyntaxPattern
compileElmSyntaxPatternListOrTupleItems compilation listItems addLengthCondition =
    case listItems of
        [] ->
            compilePatternOnlyEqualsCondition (ListExpression [])

        _ ->
            case elmSyntaxListPatternAsLiteral [] listItems of
                Just literalValues ->
                    compilePatternOnlyEqualsCondition
                        (LiteralExpression (Pine.ListValue literalValues))

                Nothing ->
                    compileElmSyntaxPatternListOrTupleItemsGeneric
                        compilation
                        listItems
                        addLengthCondition


elmSyntaxListPatternAsLiteral :
    List Pine.Value
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> Maybe (List Pine.Value)
elmSyntaxListPatternAsLiteral processedItems listItems =
    case listItems of
        currentItemPatternNode :: followingItems ->
            let
                (Elm.Syntax.Node.Node _ currentItemPattern) =
                    currentItemPatternNode
            in
            case currentItemPattern of
                Elm.Syntax.Pattern.IntPattern int ->
                    elmSyntaxListPatternAsLiteral
                        (Pine.valueFromInt int :: processedItems)
                        followingItems

                Elm.Syntax.Pattern.HexPattern int ->
                    elmSyntaxListPatternAsLiteral
                        (Pine.valueFromInt int :: processedItems)
                        followingItems

                Elm.Syntax.Pattern.StringPattern string ->
                    elmSyntaxListPatternAsLiteral
                        (valueFromString string :: processedItems)
                        followingItems

                Elm.Syntax.Pattern.CharPattern char ->
                    elmSyntaxListPatternAsLiteral
                        (Pine.valueFromChar char :: processedItems)
                        followingItems

                Elm.Syntax.Pattern.ListPattern innerListItems ->
                    case elmSyntaxListPatternAsLiteral [] innerListItems of
                        Just innerLiteralValues ->
                            elmSyntaxListPatternAsLiteral
                                (Pine.ListValue innerLiteralValues :: processedItems)
                                followingItems

                        Nothing ->
                            Nothing

                Elm.Syntax.Pattern.TuplePattern innerTupleItems ->
                    case elmSyntaxListPatternAsLiteral [] innerTupleItems of
                        Just innerLiteralValues ->
                            elmSyntaxListPatternAsLiteral
                                (Pine.ListValue innerLiteralValues :: processedItems)
                                followingItems

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        [] ->
            Just (List.reverse processedItems)


compileElmSyntaxPatternListOrTupleItemsGeneric :
    CompilationStack
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> Bool
    -> Result String CompiledElmSyntaxPattern
compileElmSyntaxPatternListOrTupleItemsGeneric compilation listItems addLengthCondition =
    case
        Common.resultListIndexedMapCombine
            (\( argIndex, Elm.Syntax.Node.Node _ itemPattern ) ->
                compileItemPatternForIndex compilation argIndex itemPattern
            )
            listItems
    of
        Err err ->
            Err err

        Ok itemsResults ->
            let
                conditionExpressions : Expression -> List Expression
                conditionExpressions deconstructedExpression =
                    let
                        matchesLengthConditions =
                            if addLengthCondition then
                                {-
                                   In contrast to a tuple pattern, we also need to check the length of the list matches the pattern.
                                   (For tuples, the length is always fixed.)
                                -}
                                let
                                    expectedLength =
                                        List.length listItems
                                in
                                [ case deconstructedExpression of
                                    ListExpression deconstructedList ->
                                        LiteralExpression
                                            (if List.length deconstructedList == expectedLength then
                                                Pine.trueValue

                                             else
                                                Pine.falseValue
                                            )

                                    _ ->
                                        equalCondition
                                            [ LiteralExpression (Pine.valueFromInt expectedLength)
                                            , countListElementsExpression deconstructedExpression
                                            ]
                                ]

                            else
                                []
                    in
                    List.concat
                        [ matchesLengthConditions
                        , List.concatMap
                            (\(CompiledElmSyntaxPattern conditions _ _) ->
                                conditions deconstructedExpression
                            )
                            itemsResults
                        ]

                aggregateCannotMatchEmpty : Bool
                aggregateCannotMatchEmpty =
                    List.any
                        (\(CompiledElmSyntaxPattern _ itemCannotMatchEmpty _) ->
                            itemCannotMatchEmpty
                        )
                        itemsResults

                aggregateDeclarations : List ( String, List Deconstruction )
                aggregateDeclarations =
                    List.concatMap
                        (\(CompiledElmSyntaxPattern _ _ declarations) ->
                            declarations
                        )
                        itemsResults
            in
            Ok
                (CompiledElmSyntaxPattern
                    conditionExpressions
                    aggregateCannotMatchEmpty
                    aggregateDeclarations
                )


compileItemPatternForIndex :
    CompilationStack
    -> Int
    -> Elm.Syntax.Pattern.Pattern
    -> Result String CompiledElmSyntaxPattern
compileItemPatternForIndex compilation itemIndex itemPattern =
    case compileElmSyntaxPattern compilation itemPattern of
        Err err ->
            Err err

        Ok (CompiledElmSyntaxPattern itemConditions itemCannotMatchEmpty itemDecls) ->
            Ok
                (CompiledElmSyntaxPattern
                    (\mapped ->
                        itemConditions
                            (listItemFromIndexExpression itemIndex mapped)
                    )
                    itemCannotMatchEmpty
                    (List.map
                        (\( declName, deconsExpr ) ->
                            ( declName
                            , ListItemDeconstruction itemIndex :: deconsExpr
                            )
                        )
                        itemDecls
                    )
                )


compilePatternOnlyEqualsCondition :
    Expression
    -> Result error CompiledElmSyntaxPattern
compilePatternOnlyEqualsCondition valueToCompare =
    Ok
        (CompiledElmSyntaxPattern
            (\deconstructedExpression ->
                [ equalCondition [ deconstructedExpression, valueToCompare ] ]
            )
            ((valueToCompare /= ListExpression [])
                && (valueToCompare /= LiteralExpression Pine.listValue_Empty)
            )
            []
        )


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
    let
        continueForEquals : (Expression -> Expression) -> Maybe (Result String Expression)
        continueForEquals mapAfterEquals =
            {-
               For the general case, '==' will be compiled to 'Basics.eq', which is polymorphic and has a more complex
               implementation to handle 'Set' and 'Dict' instances.
               If we can prove at compile time that the operand type cannot contain any 'Set' or 'Dict',
               we can emit a direct usage of the 'equal' kernel function.
            -}
            let
                operandCannotContainSetOrDict =
                    if exprCannotContainSetOrDict stack.knownTypes leftExpr then
                        True

                    else
                        exprCannotContainSetOrDict stack.knownTypes rightExpr
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
                                        (mapAfterEquals
                                            (KernelApplicationExpression
                                                "equal"
                                                (ListExpression [ leftExprCompiled, rightExprCompiled ])
                                            )
                                        )
                                    )
    in
    case operator of
        "==" ->
            {-
               For the general case, '==' will be compiled to 'Basics.eq', which is polymorphic and has a more complex
               implementation to handle 'Set' and 'Dict' instances.
               If we can prove at compile time that the operand type cannot contain any 'Set' or 'Dict',
               we can emit a direct usage of the 'equal' kernel function.
            -}
            continueForEquals identity

        "/=" ->
            continueForEquals
                (\equalExpr ->
                    KernelApplicationExpression
                        "negate"
                        equalExpr
                )

        "//" ->
            -- Operator (//) implies both operands are integers.
            let
                continueWithIntOnRight divisor =
                    case compileElmSyntaxExpression stack leftExpr of
                        Err err ->
                            Just (Err err)

                        Ok leftExprCompiled ->
                            case divisor of
                                0x0100 ->
                                    Just
                                        (Ok
                                            (ConditionalExpression
                                                (KernelApplicationExpression
                                                    "equal"
                                                    (ListExpression
                                                        [ KernelApplicationExpression "length" leftExprCompiled
                                                        , LiteralExpression (Pine.valueFromInt 2)
                                                        ]
                                                    )
                                                )
                                                (ConditionalExpression
                                                    (KernelApplicationExpression
                                                        "equal"
                                                        (ListExpression
                                                            [ KernelApplicationExpression "length" leftExprCompiled
                                                            , LiteralExpression (Pine.valueFromInt 1)
                                                            ]
                                                        )
                                                    )
                                                    (ConditionalExpression
                                                        (KernelApplicationExpression
                                                            "equal"
                                                            (ListExpression
                                                                [ KernelApplicationExpression "length" leftExprCompiled
                                                                , LiteralExpression (Pine.valueFromInt 0)
                                                                ]
                                                            )
                                                        )
                                                        (KernelApplicationExpression
                                                            "reverse"
                                                            (KernelApplicationExpression
                                                                "skip"
                                                                (ListExpression
                                                                    [ LiteralExpression (Pine.valueFromInt 1)
                                                                    , KernelApplicationExpression "reverse" leftExprCompiled
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                        (LiteralExpression Pine.listValue_Empty)
                                                    )
                                                    (LiteralExpression Pine.listValue_Empty)
                                                )
                                                (LiteralExpression (Pine.valueFromInt 0))
                                            )
                                        )

                                _ ->
                                    Nothing
            in
            case rightExpr of
                Elm.Syntax.Expression.Integer divisor ->
                    continueWithIntOnRight divisor

                Elm.Syntax.Expression.Hex divisor ->
                    continueWithIntOnRight divisor

                _ ->
                    Nothing

        "+" ->
            if exprProvenToBeInt stack.knownTypes leftExpr then
                if exprProvenToBeInt stack.knownTypes rightExpr then
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
                                                "int_add"
                                                (ListExpression
                                                    [ leftExprCompiled
                                                    , rightExprCompiled
                                                    ]
                                                )
                                            )
                                        )

                else
                    Nothing

            else
                Nothing

        "*" ->
            if exprProvenToBeInt stack.knownTypes leftExpr then
                if exprProvenToBeInt stack.knownTypes rightExpr then
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
                                                "int_mul"
                                                (ListExpression
                                                    [ leftExprCompiled
                                                    , rightExprCompiled
                                                    ]
                                                )
                                            )
                                        )

                else
                    Nothing

            else
                Nothing

        "-" ->
            if exprProvenToBeInt stack.knownTypes leftExpr then
                if exprProvenToBeInt stack.knownTypes rightExpr then
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
                                                "int_add"
                                                (ListExpression
                                                    [ leftExprCompiled
                                                    , KernelApplicationExpression "negate" rightExprCompiled
                                                    ]
                                                )
                                            )
                                        )

                else
                    Nothing

            else
                Nothing

        "++" ->
            let
                items : List Elm.Syntax.Expression.Expression
                items =
                    List.concat
                        [ flattenOperatorAppSequencePlusPlus leftExpr
                        , flattenOperatorAppSequencePlusPlus rightExpr
                        ]

                anyItemIsString : Bool
                anyItemIsString =
                    List.any
                        (exprProvenToBeString stack.knownTypes)
                        items

                anyItemIsList : Bool
                anyItemIsList =
                    List.any
                        (exprProvenToBeList stack.knownTypes)
                        items
            in
            if anyItemIsList then
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
                                            "concat"
                                            (ListExpression [ leftExprCompiled, rightExprCompiled ])
                                        )
                                    )

            else if anyItemIsString then
                case Common.resultListMapCombine (compileElmSyntaxExpression stack) items of
                    Err err ->
                        Just (Err err)

                    Ok expressions ->
                        let
                            {- Depend on the specific representation of Elm strings
                               to retrieve the lists of characters before concatenation and
                               compose the resulting 'String' after concatenation.
                            -}
                            stringsExpressions : List Expression
                            stringsExpressions =
                                List.concatMap
                                    (\stringExpr ->
                                        case stringExpr of
                                            LiteralExpression (Pine.ListValue ({- 'String' tag -} _ :: (Pine.ListValue [ Pine.ListValue literalChars ]) :: _)) ->
                                                --  Some code uses syntax like ` var ++ "" ` to simplify (local) type inference.
                                                if literalChars == [] then
                                                    []

                                                else
                                                    [ LiteralExpression
                                                        (Pine.ListValue literalChars)
                                                    ]

                                            _ ->
                                                [ FirCompiler.listItemFromIndexExpression 0
                                                    (FirCompiler.listItemFromIndexExpression 1 stringExpr)
                                                ]
                                    )
                                    expressions

                            concatExpr =
                                KernelApplicationExpression
                                    "concat"
                                    (ListExpression stringsExpressions)
                        in
                        Just
                            (Ok
                                (ListExpression
                                    [ LiteralExpression elmStringTypeTagNameAsValue
                                    , ListExpression [ concatExpr ]
                                    ]
                                )
                            )

            else
                Nothing

        "::" ->
            {-
               The Basics.(::) operator is a simple case, because it only works on lists.
               Therefore, we can emit a direct usage of the 'concat' kernel function.
            -}
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
                                        "concat"
                                        (ListExpression
                                            [ ListExpression [ leftExprCompiled ]
                                            , rightExprCompiled
                                            ]
                                        )
                                    )
                                )

        "|>" ->
            case compileElmSyntaxExpression stack leftExpr of
                Err err ->
                    Just (Err err)

                Ok leftExprCompiled ->
                    case compileElmSyntaxExpression stack rightExpr of
                        Err err ->
                            Just (Err err)

                        Ok rightExprCompiled ->
                            let
                                ( funcExpr, aggregateArguments ) =
                                    case rightExprCompiled of
                                        FunctionApplicationExpression innerFunction arguments ->
                                            ( innerFunction
                                            , List.concat [ arguments, [ leftExprCompiled ] ]
                                            )

                                        _ ->
                                            ( rightExprCompiled, [ leftExprCompiled ] )
                            in
                            Just
                                (Ok
                                    (FunctionApplicationExpression
                                        funcExpr
                                        aggregateArguments
                                    )
                                )

        ">" ->
            if exprProvenToBeInt stack.knownTypes leftExpr then
                if exprProvenToBeInt stack.knownTypes rightExpr then
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
                                                "negate"
                                                (KernelApplicationExpression
                                                    "int_is_sorted_asc"
                                                    (ListExpression [ leftExprCompiled, rightExprCompiled ])
                                                )
                                            )
                                        )

                else
                    Nothing

            else
                Nothing

        ">=" ->
            if exprProvenToBeInt stack.knownTypes leftExpr then
                if exprProvenToBeInt stack.knownTypes rightExpr then
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
                                                "int_is_sorted_asc"
                                                (ListExpression [ rightExprCompiled, leftExprCompiled ])
                                            )
                                        )

                else
                    Nothing

            else
                Nothing

        "<" ->
            if exprProvenToBeInt stack.knownTypes leftExpr then
                if exprProvenToBeInt stack.knownTypes rightExpr then
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
                                                "negate"
                                                (KernelApplicationExpression
                                                    "int_is_sorted_asc"
                                                    (ListExpression [ rightExprCompiled, leftExprCompiled ])
                                                )
                                            )
                                        )

                else
                    Nothing

            else
                Nothing

        "<=" ->
            if exprProvenToBeInt stack.knownTypes leftExpr then
                if exprProvenToBeInt stack.knownTypes rightExpr then
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
                                                "int_is_sorted_asc"
                                                (ListExpression [ leftExprCompiled, rightExprCompiled ])
                                            )
                                        )

                else
                    Nothing

            else
                Nothing

        _ ->
            Nothing


exprCannotContainSetOrDict :
    List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Elm.Syntax.Expression.Expression
    -> Bool
exprCannotContainSetOrDict knownTypes expr =
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
            List.all
                (\(Elm.Syntax.Node.Node _ listItem) ->
                    exprCannotContainSetOrDict knownTypes listItem
                )
                listExpr

        Elm.Syntax.Expression.TupledExpression tupleExpr ->
            List.all
                (\(Elm.Syntax.Node.Node _ listItem) ->
                    exprCannotContainSetOrDict knownTypes listItem
                )
                tupleExpr

        Elm.Syntax.Expression.FunctionOrValue _ localName ->
            -- Choice type tags without arguments, like 'Nothing' or 'LT'.
            if stringStartsWithUpper localName then
                True

            else
                nameCannotContainSetOrDict knownTypes localName

        Elm.Syntax.Expression.Application ((Elm.Syntax.Node.Node _ functionExpr) :: argumentsNodes) ->
            case functionExpr of
                Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
                    if stringStartsWithUpper localName then
                        -- Choice type tags without arguments, like 'Just' or 'Err'.
                        List.all
                            (\(Elm.Syntax.Node.Node _ arg) ->
                                exprCannotContainSetOrDict knownTypes arg
                            )
                            argumentsNodes

                    else
                        functionCannotReturnSetOrDict ( moduleName, localName )

                _ ->
                    False

        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ parenthesized) ->
            exprCannotContainSetOrDict knownTypes parenthesized

        _ ->
            if exprProvenToBeInt knownTypes expr then
                True

            else if exprProvenToBeString knownTypes expr then
                True

            else
                exprProvenToBeChar knownTypes expr


nameCannotContainSetOrDict : List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation ) -> String -> Bool
nameCannotContainSetOrDict knownTypes localName =
    case Common.assocListGet localName knownTypes of
        Just knownType ->
            typeCannotContainSetOrDict knownTypes knownType

        _ ->
            False


typeCannotContainSetOrDict :
    List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Bool
typeCannotContainSetOrDict knownTypes typeAnnotation =
    case typeAnnotation of
        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( [], typeName )) typeArgs ->
            case ( typeName, typeArgs ) of
                ( "Int", [] ) ->
                    True

                ( "String", [] ) ->
                    True

                ( "Char", [] ) ->
                    True

                ( "List", [ Elm.Syntax.Node.Node _ itemType ] ) ->
                    typeCannotContainSetOrDict knownTypes itemType

                _ ->
                    True

        _ ->
            False


functionCannotReturnSetOrDict : ( List String, String ) -> Bool
functionCannotReturnSetOrDict ( moduleName, localName ) =
    if functionProvenToReturnInt ( moduleName, localName ) then
        True

    else
        case ( moduleName, localName ) of
            ( [ "Basics" ], "compare" ) ->
                True

            _ ->
                False


exprProvenToBeInt :
    List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Elm.Syntax.Expression.Expression
    -> Bool
exprProvenToBeInt knownTypes expr =
    case expr of
        Elm.Syntax.Expression.Integer _ ->
            True

        Elm.Syntax.Expression.Hex _ ->
            True

        Elm.Syntax.Expression.FunctionOrValue [] localName ->
            case Common.assocListGet localName knownTypes of
                Just (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( [], "Int" )) []) ->
                    True

                _ ->
                    False

        Elm.Syntax.Expression.Application ((Elm.Syntax.Node.Node _ (Elm.Syntax.Expression.FunctionOrValue moduleName localName)) :: _) ->
            functionProvenToReturnInt
                ( moduleName, localName )

        Elm.Syntax.Expression.OperatorApplication innerOp _ (Elm.Syntax.Node.Node _ innerLeft) (Elm.Syntax.Node.Node _ innerRight) ->
            case innerOp of
                "+" ->
                    if exprProvenToBeInt knownTypes innerLeft then
                        exprProvenToBeInt knownTypes innerRight

                    else
                        False

                "-" ->
                    if exprProvenToBeInt knownTypes innerLeft then
                        exprProvenToBeInt knownTypes innerRight

                    else
                        False

                "*" ->
                    if exprProvenToBeInt knownTypes innerLeft then
                        exprProvenToBeInt knownTypes innerRight

                    else
                        False

                "//" ->
                    True

                _ ->
                    False

        _ ->
            False


exprProvenToBeList :
    List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Elm.Syntax.Expression.Expression
    -> Bool
exprProvenToBeList knownTypes expr =
    case expr of
        Elm.Syntax.Expression.ListExpr _ ->
            True

        Elm.Syntax.Expression.FunctionOrValue [] localName ->
            case Common.assocListGet localName knownTypes of
                Just (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( [], "List" )) [ Elm.Syntax.Node.Node _ _ ]) ->
                    True

                _ ->
                    False

        _ ->
            False


exprProvenToBeString :
    List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Elm.Syntax.Expression.Expression
    -> Bool
exprProvenToBeString knownTypes expr =
    case expr of
        Elm.Syntax.Expression.Literal _ ->
            True

        Elm.Syntax.Expression.FunctionOrValue [] localName ->
            case Common.assocListGet localName knownTypes of
                Just (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( [], "String" )) []) ->
                    True

                _ ->
                    False

        Elm.Syntax.Expression.OperatorApplication innerOp _ (Elm.Syntax.Node.Node _ innerLeft) (Elm.Syntax.Node.Node _ innerRight) ->
            case innerOp of
                "++" ->
                    if exprProvenToBeString knownTypes innerLeft then
                        True

                    else
                        exprProvenToBeString knownTypes innerRight

                _ ->
                    False

        Elm.Syntax.Expression.Application ((Elm.Syntax.Node.Node _ (Elm.Syntax.Expression.FunctionOrValue moduleName localName)) :: argumentsNodes) ->
            funcAppProvenToReturnString
                ( ( moduleName, localName ), List.length argumentsNodes )

        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ parenthesized) ->
            exprProvenToBeString knownTypes parenthesized

        _ ->
            False


exprProvenToBeChar :
    List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Elm.Syntax.Expression.Expression
    -> Bool
exprProvenToBeChar knownTypes expr =
    case expr of
        Elm.Syntax.Expression.CharLiteral _ ->
            True

        Elm.Syntax.Expression.FunctionOrValue [] localName ->
            case Common.assocListGet localName knownTypes of
                Just (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( [], "Char" )) []) ->
                    True

                _ ->
                    False

        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ parenthesized) ->
            exprProvenToBeChar knownTypes parenthesized

        _ ->
            False


functionProvenToReturnInt : ( List String, String ) -> Bool
functionProvenToReturnInt ( moduleName, localName ) =
    case ( moduleName, localName ) of
        ( [ "List" ], "length" ) ->
            True

        ( [ "String" ], "length" ) ->
            True

        ( [ "Pine_kernel" ], "length" ) ->
            -- Possibly another stage has already mapped from 'List.length' to 'Pine_kernel.length'.
            True

        _ ->
            False


funcAppProvenToReturnString : ( ( List String, String ), Int ) -> Bool
funcAppProvenToReturnString application =
    case application of
        ( ( [ "String" ], "fromList" ), 1 ) ->
            True

        ( ( [ "String" ], "slice" ), 3 ) ->
            True

        ( ( [ "String" ], "cons" ), 2 ) ->
            True

        ( ( [ "String" ], "fromInt" ), 1 ) ->
            True

        ( ( [ "String" ], "fromFloat" ), 1 ) ->
            True

        ( ( [ "String" ], "fromChar" ), 1 ) ->
            True

        ( ( [ "String" ], "toLower" ), 1 ) ->
            True

        ( ( [ "String" ], "toUpper" ), 1 ) ->
            True

        _ ->
            False


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
            pineKernel_Head_Pine (listItemFromIndexExpression_Pine 1 recordExpression)

        fieldsUpdatesExpression =
            listItemFromIndexExpression_Pine 1 Pine.environmentExpr

        recursiveFunction : Pine.Value
        recursiveFunction =
            Pine.encodeExpressionAsValue recursiveFunctionToUpdateFieldsInRecord
    in
    Pine.ConditionalExpression
        (equalCondition_Pine
            [ Pine.LiteralExpression elmRecordTypeTagNameAsValue
            , pineKernel_Head_Pine recordExpression
            ]
        )
        (Pine.ParseAndEvalExpression
            (Pine.LiteralExpression
                (Pine.valueFromString "invalid record update - not a record")
            )
            recordExpression
        )
        (Pine.ListExpression
            [ Pine.LiteralExpression elmRecordTypeTagNameAsValue
            , Pine.ListExpression
                [ Pine.ParseAndEvalExpression
                    (Pine.LiteralExpression recursiveFunction)
                    (Pine.ListExpression
                        [ Pine.LiteralExpression recursiveFunction
                        , fieldsUpdatesExpression
                        , Pine.ListExpression []
                        , recordFieldsExpression
                        ]
                    )
                ]
            ]
        )


{-| Recursively scans through the record fields and replaces every field contained in the argument list.
The argument list contains pairs of field names and new values.

Takes the following arguments:

1.  The function itself, so that we don't have to depend on recursion in the environment.
2.  A list of field updates, each as a tuple of field name and new value. The call site is responsible for sorting this list by field name. If the list of updates is not in the correct order, this function crashes at runtime.
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
        (equalCondition_Pine
            [ Pine.ListExpression []
            , fieldPairsLocalExpression
            ]
        )
        (Pine.ConditionalExpression
            (equalCondition_Pine
                [ Pine.ListExpression []
                , remainingFieldsLocalExpression
                ]
            )
            (Pine.ConditionalExpression
                (equalCondition_Pine
                    [ listItemFromIndexExpression_Pine 0 remainingFieldsNextLocalExpression
                    , firstFieldNameLocalExpression
                    ]
                )
                (Pine.ParseAndEvalExpression
                    functionReferenceLocalExpression
                    (Pine.ListExpression
                        [ functionReferenceLocalExpression
                        , fieldPairsLocalExpression
                        , Pine.KernelApplicationExpression
                            "concat"
                            (Pine.ListExpression
                                [ processedFieldsLocalExpression
                                , Pine.ListExpression
                                    [ remainingFieldsNextLocalExpression ]
                                ]
                            )
                        , listSkipExpression_Pine 1 remainingFieldsLocalExpression
                        ]
                    )
                )
                (Pine.ParseAndEvalExpression
                    functionReferenceLocalExpression
                    (Pine.ListExpression
                        [ functionReferenceLocalExpression
                        , listSkipExpression_Pine 1 fieldPairsLocalExpression
                        , Pine.KernelApplicationExpression
                            "concat"
                            (Pine.ListExpression
                                [ processedFieldsLocalExpression
                                , Pine.ListExpression
                                    [ firstFieldPairLocalExpression ]
                                ]
                            )
                        , listSkipExpression_Pine 1 remainingFieldsLocalExpression
                        ]
                    )
                )
            )
            (Pine.ParseAndEvalExpression
                (Pine.LiteralExpression
                    (Pine.valueFromString "invalid record update - field name not found")
                )
                firstFieldNameLocalExpression
            )
        )
        (Pine.KernelApplicationExpression
            "concat"
            (Pine.ListExpression
                [ processedFieldsLocalExpression
                , remainingFieldsLocalExpression
                ]
            )
        )


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
            pineKernel_Head_Pine (listItemFromIndexExpression_Pine 1 recordExpression)
    in
    Pine.ConditionalExpression
        (equalCondition_Pine
            [ Pine.LiteralExpression elmRecordTypeTagNameAsValue
            , pineKernel_Head_Pine recordExpression
            ]
        )
        (Pine.ParseAndEvalExpression
            (Pine.LiteralExpression
                (Pine.valueFromString "invalid record access - not a record")
            )
            fieldNameLocalExpression
        )
        (Pine.ParseAndEvalExpression
            (Pine.LiteralExpression recursiveFunctionToLookupFieldInRecordAsValue)
            (Pine.ListExpression
                [ Pine.LiteralExpression recursiveFunctionToLookupFieldInRecordAsValue
                , fieldNameLocalExpression
                , recordFieldsExpression
                ]
            )
        )


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
                selfFunctionLocalExpression
                (Pine.ListExpression
                    [ selfFunctionLocalExpression
                    , fieldNameLocalExpression
                    , listSkipExpression_Pine 1 remainingFieldsLocalExpression
                    ]
                )
    in
    Pine.ConditionalExpression
        (equalCondition_Pine
            [ Pine.ListExpression []
            , remainingFieldsLocalExpression
            ]
        )
        (Pine.ConditionalExpression
            (equalCondition_Pine
                [ remainingFieldsLocalExpression
                    |> listItemFromIndexExpression_Pine 0
                    |> listItemFromIndexExpression_Pine 0
                , fieldNameLocalExpression
                ]
            )
            continueWithRemainingExpression
            (remainingFieldsLocalExpression
                |> listItemFromIndexExpression_Pine 0
                |> listItemFromIndexExpression_Pine 1
            )
        )
        (Pine.ParseAndEvalExpression
            (Pine.LiteralExpression
                (Pine.valueFromString "invalid record access - field name not found")
            )
            fieldNameLocalExpression
        )


compileElmFunctionOrValueLookup : ( List String, String ) -> CompilationStack -> Result String Expression
compileElmFunctionOrValueLookup ( moduleNameBeforeImport, declName ) compilation =
    case sourceModuleNameFromImports ( moduleNameBeforeImport, declName ) compilation of
        Err err ->
            Err err

        Ok moduleName ->
            case tryApplyNamedInlineableOrTypeDecl moduleName declName [] compilation of
                Just fromTypeDecl ->
                    Ok fromTypeDecl

                Nothing ->
                    case moduleName of
                        [] ->
                            compileElmFunctionOrValueLookupWithoutLocalResolution ( moduleName, declName ) compilation

                        _ ->
                            case getDeclarationValueFromCompilation ( moduleName, declName ) compilation of
                                Err err ->
                                    Err err

                                Ok declarationValue ->
                                    Ok (compileLookupForInlineableDeclaration ( moduleName, declName ) declarationValue)


compileElmFunctionOrValueLookupWithoutLocalResolution :
    ( List String, String )
    -> CompilationStack
    -> Result String Expression
compileElmFunctionOrValueLookupWithoutLocalResolution ( moduleName, declName ) compilation =
    case Common.assocListGet declName elmDeclarationsOverridesExpressions of
        Just declarationOverride ->
            Ok declarationOverride

        Nothing ->
            case moduleName of
                [] ->
                    if List.member declName compilation.localAvailableDeclarations then
                        Ok (ReferenceExpression [] declName)

                    else
                        Ok (ReferenceExpression moduleName declName)

                _ ->
                    Ok (ReferenceExpression moduleName declName)


getDeclarationValueFromCompilation : ( List String, String ) -> CompilationStack -> Result String Expression
getDeclarationValueFromCompilation ( moduleName, nameInModule ) compilation =
    case Dict.get moduleName compilation.availableModules of
        Nothing ->
            Err
                ("Did not find module '"
                    ++ String.join "." moduleName
                    ++ "'. There are "
                    ++ String.fromInt (Dict.size compilation.availableModules)
                    ++ " modules available: "
                    ++ String.join ", " (List.map (String.join ".") (Dict.keys compilation.availableModules))
                )

        Just moduleValue ->
            case Common.assocListGet nameInModule moduleValue.functionDeclarations of
                Just declarationValue ->
                    Ok (LiteralExpression declarationValue)

                Nothing ->
                    let
                        declsReport =
                            if stringStartsWithUpper nameInModule then
                                let
                                    allTypesNames =
                                        List.foldl
                                            (\( typeName, value ) aggregate ->
                                                case value of
                                                    ElmModuleChoiceTypeDeclaration (ElmModuleChoiceType choiceTypeTags) ->
                                                        List.concat
                                                            [ [ typeName ]
                                                            , List.map Tuple.first choiceTypeTags
                                                            , aggregate
                                                            ]

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
                                    ++ String.fromInt (List.length moduleValue.functionDeclarations)
                                    ++ " function declarations available in that module: "
                                    ++ String.join ", " (List.map Tuple.first moduleValue.functionDeclarations)
                    in
                    Err
                        ("Did not find '"
                            ++ nameInModule
                            ++ "' in module '"
                            ++ String.join "." moduleName
                            ++ "'. "
                            ++ declsReport
                        )


sourceModuleNameFromImports : ( List String, String ) -> CompilationStack -> Result String (List String)
sourceModuleNameFromImports ( moduleName, declName ) compilation =
    case moduleName of
        [] ->
            if List.member declName compilation.localAvailableDeclarations then
                Ok []

            else
                case Common.assocListGet declName compilation.localTypeDeclarations of
                    Just _ ->
                        Ok []

                    Nothing ->
                        case Common.assocListGet declName compilation.exposedDeclarations of
                            Just moduleNames ->
                                case moduleNames of
                                    [ singleModuleName ] ->
                                        Ok singleModuleName

                                    _ ->
                                        Err
                                            (String.join ""
                                                [ "Ambiguous reference to '"
                                                , declName
                                                , "': Found "
                                                , String.fromInt (List.length moduleNames)
                                                , " matching modules: "
                                                , String.join ", " (List.map (String.join ".") moduleNames)
                                                ]
                                            )

                            Nothing ->
                                Ok moduleName

        _ ->
            case Dict.get moduleName compilation.moduleAliases of
                Nothing ->
                    Ok moduleName

                Just aliasedModuleName ->
                    Ok aliasedModuleName


compileLookupForInlineableDeclaration : ( List String, String ) -> Expression -> Expression
compileLookupForInlineableDeclaration ( moduleName, name ) expression =
    if shouldInlineDeclaration name expression then
        expression

    else
        ReferenceExpression moduleName name


{-| Encodes an Elm module into a transportable form.
-}
emitModuleValue : ElmModuleInCompilation -> Pine.Value
emitModuleValue parsedModule =
    let
        typeDescriptions : List ( String, Pine.Value )
        typeDescriptions =
            List.foldr
                (\( typeName, typeDeclaration ) aggregate ->
                    ( typeName, emitTypeDeclarationValue typeDeclaration ) :: aggregate
                )
                []
                parsedModule.typeDeclarations
    in
    Pine.ListValue
        (List.map Pine.valueFromContextExpansionWithName
            (List.concat [ parsedModule.functionDeclarations, typeDescriptions ])
        )


emitTypeDeclarationValue : ElmModuleTypeDeclaration -> Pine.Value
emitTypeDeclarationValue typeDeclaration =
    case typeDeclaration of
        ElmModuleChoiceTypeDeclaration choiceType ->
            emitChoiceTypeValue choiceType

        ElmModuleRecordTypeDeclaration fields ->
            emitRecordConstructorValue fields


emitChoiceTypeValue : ElmModuleChoiceType -> Pine.Value
emitChoiceTypeValue (ElmModuleChoiceType tags) =
    Pine.valueFromContextExpansionWithName
        ( "ChoiceType"
        , Pine.ListValue
            (List.foldr
                (\( tagName, ElmModuleChoiceTypeTag argumentsCount ) aggregate ->
                    Pine.ListValue
                        [ Pine.valueFromString tagName
                        , Pine.valueFromInt argumentsCount
                        ]
                        :: aggregate
                )
                []
                tags
            )
        )


emitRecordConstructorValue : List String -> Pine.Value
emitRecordConstructorValue fields =
    Pine.valueFromContextExpansionWithName
        ( "RecordConstructor"
        , Pine.ListValue
            (List.map Pine.valueFromString fields)
        )


type alias EmittedRecursionDomain =
    { emittedDeclarationsToShare : List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
    , emittedDeclarationsToInline : List ( String, Pine.Value )
    , exposedDeclarations : List ( String, Pine.Value )
    }


emitModuleFunctionDeclarations :
    EmitStack
    -> List ( String, Expression )
    -> List String
    -> Result String (List ( String, Pine.Value ))
emitModuleFunctionDeclarations emitStack allModuleDeclarations exposedDeclarationsNames =
    let
        declarationsDirectDependencies : Dict.Dict String (List String)
        declarationsDirectDependencies =
            List.foldl
                (\( declName, declExpr ) aggregate ->
                    Dict.insert
                        declName
                        (Common.listUnique (FirCompiler.listUnboundReferencesInExpression declExpr []))
                        aggregate
                )
                Dict.empty
                allModuleDeclarations

        aggregateTransitiveDependencies : List String
        aggregateTransitiveDependencies =
            FirCompiler.getTransitiveDependencies
                declarationsDirectDependencies
                exposedDeclarationsNames

        declarationsTransitiveDependencies : List ( String, Set.Set String )
        declarationsTransitiveDependencies =
            Dict.foldl
                (\declarationName directDependencies aggregate ->
                    if List.member declarationName aggregateTransitiveDependencies then
                        ( declarationName
                        , Set.fromList
                            (FirCompiler.getTransitiveDependencies
                                declarationsDirectDependencies
                                directDependencies
                            )
                        )
                            :: aggregate

                    else
                        aggregate
                )
                []
                declarationsDirectDependencies

        ( importedFunctionsToShare, importedFunctionsToInline ) =
            List.foldl
                (\( moduleName, moduleDecls ) ( toShareBefore, toInlineBefore ) ->
                    let
                        ( moduleDeclsToShare, moduleDeclsToInline ) =
                            splitEmittedFunctionsToInline moduleDecls
                    in
                    ( ( moduleName, moduleDeclsToShare ) :: toShareBefore
                    , ( moduleName, moduleDeclsToInline ) :: toInlineBefore
                    )
                )
                ( [], [] )
                emitStack.importedFunctions

        recursionDomains : List (Set.Set String)
        recursionDomains =
            FirCompiler.recursionDomainsFromDeclarationDependencies
                declarationsTransitiveDependencies

        emitRecursionDomainsRecursive :
            List EmittedRecursionDomain
            -> List (Set.Set String)
            -> Result String (List EmittedRecursionDomain)
        emitRecursionDomainsRecursive alreadyEmitted remainingRecursionDomains =
            case remainingRecursionDomains of
                [] ->
                    Ok alreadyEmitted

                currentRecursionDomain :: followingRecursionDomains ->
                    case
                        emitRecursionDomain
                            { exposedDeclarationsNames = exposedDeclarationsNames
                            , allModuleDeclarations = allModuleDeclarations
                            , importedFunctionsToShare = importedFunctionsToShare
                            , importedFunctionsToInline = importedFunctionsToInline
                            , declarationsDirectDependencies = declarationsDirectDependencies
                            }
                            emitStack
                            currentRecursionDomain
                            alreadyEmitted
                    of
                        Err err ->
                            Err err

                        Ok emittedDomain ->
                            emitRecursionDomainsRecursive
                                (List.concat [ alreadyEmitted, [ emittedDomain ] ])
                                followingRecursionDomains
    in
    case emitRecursionDomainsRecursive [] recursionDomains of
        Err err ->
            Err err

        Ok domains ->
            Ok (List.concatMap .exposedDeclarations domains)


emitRecursionDomain :
    { exposedDeclarationsNames : List String
    , allModuleDeclarations : List ( String, Expression )
    , importedFunctionsToShare : List ( List String, List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) ) )
    , importedFunctionsToInline : List ( List String, List ( String, Pine.Value ) )
    , declarationsDirectDependencies : Dict.Dict String (List String)
    }
    -> EmitStack
    -> Set.Set String
    -> List EmittedRecursionDomain
    -> Result String EmittedRecursionDomain
emitRecursionDomain { exposedDeclarationsNames, allModuleDeclarations, importedFunctionsToShare, importedFunctionsToInline, declarationsDirectDependencies } emitStack currentRecursionDomain alreadyEmitted =
    let
        recursionDomainExposedNames : List String
        recursionDomainExposedNames =
            List.filter
                (\declName -> Set.member declName currentRecursionDomain)
                exposedDeclarationsNames

        recursionDomainDeclarations : List ( String, Expression )
        recursionDomainDeclarations =
            List.foldr
                (\( declName, declExpr ) aggregate ->
                    if Set.member declName currentRecursionDomain then
                        ( declName, declExpr ) :: aggregate

                    else
                        aggregate
                )
                []
                allModuleDeclarations

        prevEmittedDeclarationsToShare : List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
        prevEmittedDeclarationsToShare =
            List.concatMap
                (\emittedDomain -> emittedDomain.emittedDeclarationsToShare)
                alreadyEmitted

        prevEmittedDeclarationsToInline : List ( String, Pine.Value )
        prevEmittedDeclarationsToInline =
            List.concatMap
                (\emittedDomain -> emittedDomain.emittedDeclarationsToInline)
                alreadyEmitted

        recursionDomainDeclarationsToIncludeInBlock : Set.Set String
        recursionDomainDeclarationsToIncludeInBlock =
            Set.foldl
                (\declName aggregate ->
                    case Dict.get declName declarationsDirectDependencies of
                        Nothing ->
                            aggregate

                        Just directDependencies ->
                            List.foldl Set.insert aggregate directDependencies
                )
                Set.empty
                currentRecursionDomain

        recursionDomainDeclarationsInBlock : List ( String, Expression )
        recursionDomainDeclarationsInBlock =
            List.filter
                (\( declName, _ ) ->
                    Set.member declName recursionDomainDeclarationsToIncludeInBlock
                )
                recursionDomainDeclarations

        importedFunctions : List ( List String, List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) ) )
        importedFunctions =
            List.concat
                [ [ ( [], prevEmittedDeclarationsToShare ) ]
                , importedFunctionsToShare
                ]
    in
    case
        FirCompiler.emitDeclarationBlock
            { importedFunctions = importedFunctions
            , importedFunctionsToInline =
                ( [], prevEmittedDeclarationsToInline ) :: importedFunctionsToInline
            , environmentFunctions = emitStack.environmentFunctions
            , environmentDeconstructions = emitStack.environmentDeconstructions
            }
            recursionDomainDeclarationsInBlock
            (FirCompiler.DeclBlockClosureCaptures [])
            (FirCompiler.DeclBlockRootDeps (List.map Tuple.second recursionDomainDeclarations))
    of
        Err err ->
            Err err

        Ok ( blockEmitStack, declBlockResult ) ->
            let
                (FirCompiler.EmitDeclarationBlockResult newEnvFunctionsValues parseAndEmitFunction envFunctionsExpression) =
                    declBlockResult
            in
            case
                recursionDomainDeclarations
                    |> Common.resultListMapCombine
                        (\( declName, declExpression ) ->
                            case Common.assocListGetWithIndex ( [], declName ) blockEmitStack.environmentFunctions of
                                Just ( _, FirCompiler.EnvironmentFunctionEntry declParamCount _ ) ->
                                    case Common.assocListGet declName newEnvFunctionsValues of
                                        Nothing ->
                                            Err ("Compiler error: Missing entry: " ++ declName)

                                        Just ( _, ( _, declEmittedValue ) ) ->
                                            Ok
                                                ( declName
                                                , ( declExpression
                                                  , ( declParamCount
                                                    , declEmittedValue
                                                    )
                                                  )
                                                )

                                Nothing ->
                                    let
                                        ( FirCompiler.DeclBlockFunctionEntry parsedDeclarationParams _, emitDeclarationResult ) =
                                            parseAndEmitFunction declExpression
                                    in
                                    case emitDeclarationResult of
                                        Err err ->
                                            Err err

                                        Ok declEmittedExpr ->
                                            let
                                                innerExpressionValue =
                                                    Pine.encodeExpressionAsValue declEmittedExpr
                                            in
                                            Ok
                                                ( declName
                                                , ( declExpression
                                                  , ( List.length parsedDeclarationParams
                                                    , innerExpressionValue
                                                    )
                                                  )
                                                )
                        )
            of
                Err err ->
                    Err
                        ("Failed in recursion domain: "
                            ++ String.join ", " (Set.toList currentRecursionDomain)
                            ++ ": "
                            ++ err
                            ++ "\navailableFunctionsValues:\n"
                            ++ String.join "\n"
                                (reportEmittedDeclarationsForErrorMsg prevEmittedDeclarationsToShare)
                        )

                Ok emittedForExposeOrReuse ->
                    let
                        expectedEnvironmentFunctions : List ( List String, String )
                        expectedEnvironmentFunctions =
                            List.map
                                Tuple.first
                                blockEmitStack.environmentFunctions

                        emittedDeclarationsFromBlock : List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
                        emittedDeclarationsFromBlock =
                            List.map
                                (\( functionName, ( funcEntry, ( _, funcValue ) ) ) ->
                                    ( functionName, ( funcEntry, funcValue ) )
                                )
                                newEnvFunctionsValues

                        emittedDeclarationsFromBlockNames : List String
                        emittedDeclarationsFromBlockNames =
                            List.map Tuple.first emittedDeclarationsFromBlock

                        emittedDeclarationsFromExposed : List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
                        emittedDeclarationsFromExposed =
                            List.map
                                (\( functionName, ( _, ( parameterCount, funcValue ) ) ) ->
                                    ( functionName
                                    , ( FirCompiler.EnvironmentFunctionEntry
                                            parameterCount
                                            (FirCompiler.LocalEnvironment expectedEnvironmentFunctions)
                                      , funcValue
                                      )
                                    )
                                )
                                emittedForExposeOrReuse

                        emittedDeclarationsBeforeReduce : List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
                        emittedDeclarationsBeforeReduce =
                            List.concat
                                [ emittedDeclarationsFromBlock
                                , List.filter
                                    (\( functionName, _ ) ->
                                        not (List.member functionName emittedDeclarationsFromBlockNames)
                                    )
                                    emittedDeclarationsFromExposed
                                ]

                        emittedDeclarations : List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
                        emittedDeclarations =
                            List.map (Tuple.mapSecond (attemptReduceBlockDecl declBlockResult))
                                emittedDeclarationsBeforeReduce

                        ( emittedDeclarationsToShare, emittedDeclarationsToInline ) =
                            splitEmittedFunctionsToInline emittedDeclarations

                        exposedDeclarationsResult : Result String (List ( String, Pine.Value ))
                        exposedDeclarationsResult =
                            List.foldr
                                (\( declName, ( _, ( parameterCount, emittedValue ) ) ) aggregateResult ->
                                    if List.member declName recursionDomainExposedNames then
                                        case aggregateResult of
                                            Err err ->
                                                Err err

                                            Ok aggregate ->
                                                case Common.assocListGet declName emittedDeclarationsToInline of
                                                    Just toInline ->
                                                        Ok (( declName, toInline ) :: aggregate)

                                                    Nothing ->
                                                        let
                                                            getFunctionInnerExpressionFromIndex : Int -> Pine.Expression
                                                            getFunctionInnerExpressionFromIndex declarationIndex =
                                                                let
                                                                    getEnvFunctionsExpression =
                                                                        listItemFromIndexExpression_Pine 0
                                                                            Pine.environmentExpr
                                                                in
                                                                Pine.LiteralExpression
                                                                    (Pine.encodeExpressionAsValue
                                                                        (Pine.ParseAndEvalExpression
                                                                            (FirCompiler.listItemFromIndexExpression_Pine
                                                                                declarationIndex
                                                                                getEnvFunctionsExpression
                                                                            )
                                                                            Pine.environmentExpr
                                                                        )
                                                                    )

                                                            getFunctionInnerExpression : Pine.Expression
                                                            getFunctionInnerExpression =
                                                                case Common.assocListGetWithIndex ( [], declName ) blockEmitStack.environmentFunctions of
                                                                    Just ( indexInBlock, _ ) ->
                                                                        getFunctionInnerExpressionFromIndex indexInBlock

                                                                    Nothing ->
                                                                        Pine.LiteralExpression emittedValue
                                                        in
                                                        case
                                                            evaluateAsIndependentExpression
                                                                (if parameterCount == 0 && newEnvFunctionsValues == [] then
                                                                    FirCompiler.emitWrapperForPartialApplicationZero
                                                                        { getFunctionInnerExpression = getFunctionInnerExpression
                                                                        , getEnvFunctionsExpression = envFunctionsExpression
                                                                        }

                                                                 else
                                                                    FirCompiler.buildRecordOfPartiallyAppliedFunction
                                                                        { getFunctionInnerExpression = getFunctionInnerExpression
                                                                        , parameterCount = parameterCount
                                                                        , getEnvFunctionsExpression = envFunctionsExpression
                                                                        , argumentsAlreadyCollected = []
                                                                        }
                                                                )
                                                        of
                                                            Err err ->
                                                                Err
                                                                    ("Failed for declaration '"
                                                                        ++ declName
                                                                        ++ "': "
                                                                        ++ err
                                                                    )

                                                            Ok wrappedForExpose ->
                                                                Ok (( declName, wrappedForExpose ) :: aggregate)

                                    else
                                        aggregateResult
                                )
                                (Ok [])
                                emittedForExposeOrReuse
                    in
                    case exposedDeclarationsResult of
                        Err err ->
                            Err err

                        Ok exposedDeclarations ->
                            Ok
                                { emittedDeclarationsToShare = emittedDeclarationsToShare
                                , emittedDeclarationsToInline = emittedDeclarationsToInline
                                , exposedDeclarations = exposedDeclarations
                                }


attemptReduceBlockDecl :
    FirCompiler.EmitDeclarationBlockResult
    -> ( FirCompiler.EnvironmentFunctionEntry, Pine.Value )
    -> ( FirCompiler.EnvironmentFunctionEntry, Pine.Value )
attemptReduceBlockDecl (FirCompiler.EmitDeclarationBlockResult _ _ envFunctionsExpression) blockDecl =
    let
        ( FirCompiler.EnvironmentFunctionEntry funcParamCount _, funcValue ) =
            blockDecl
    in
    case funcParamCount of
        0 ->
            let
                evaluatableExpr =
                    FirCompiler.emitWrapperForPartialApplicationZero
                        { getFunctionInnerExpression = Pine.LiteralExpression funcValue
                        , getEnvFunctionsExpression = envFunctionsExpression
                        }
            in
            case evaluateAsIndependentExpression evaluatableExpr of
                Ok reducedValue ->
                    let
                        reducedExpr =
                            Pine.LiteralExpression reducedValue
                    in
                    if estimatePineValueSize reducedValue > estimatePineValueSize funcValue then
                        blockDecl

                    else
                        ( FirCompiler.EnvironmentFunctionEntry 0 (FirCompiler.LocalEnvironment [])
                        , Pine.encodeExpressionAsValue reducedExpr
                        )

                Err _ ->
                    blockDecl

        _ ->
            blockDecl


splitEmittedFunctionsToInline :
    List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
    -> ( List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) ), List ( String, Pine.Value ) )
splitEmittedFunctionsToInline emittedFunctions =
    List.foldr
        (\emittedFunction ( toShare, toInline ) ->
            let
                ( declName, ( FirCompiler.EnvironmentFunctionEntry paramCount expectedEnv, declValue ) ) =
                    emittedFunction

                originalSize =
                    estimatePineValueSize declValue

                continueSharing :
                    ( List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
                    , List ( String, Pine.Value )
                    )
                continueSharing =
                    ( emittedFunction :: toShare, toInline )

                continueWithReduction :
                    ()
                    ->
                        ( List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
                        , List ( String, Pine.Value )
                        )
                continueWithReduction () =
                    case Pine.parseExpressionFromValue declValue of
                        Err _ ->
                            {- This happened for an imported declaration 'Dict.empty'.
                               Maybe this happens for all declarations imported from parsed modules?
                               Just using the original value when parsing fails seemed to work and not cause any problems.
                            -}
                            if originalSize < 30 * 1000 then
                                ( toShare, ( declName, declValue ) :: toInline )

                            else
                                continueSharing

                        Ok parsedExpr ->
                            case evaluateAsIndependentExpression parsedExpr of
                                Err _ ->
                                    continueSharing

                                Ok evaluatedValue ->
                                    let
                                        reducedSize =
                                            estimatePineValueSize evaluatedValue
                                    in
                                    if reducedSize < 30 * 1000 then
                                        ( toShare, ( declName, evaluatedValue ) :: toInline )

                                    else
                                        continueSharing
            in
            case paramCount of
                0 ->
                    case expectedEnv of
                        FirCompiler.IndependentEnvironment ->
                            continueWithReduction ()

                        FirCompiler.LocalEnvironment [] ->
                            continueWithReduction ()

                        _ ->
                            continueSharing

                _ ->
                    continueSharing
        )
        ( [], [] )
        emittedFunctions


reportEmittedDeclarationsForErrorMsg :
    List ( String, ( FirCompiler.EnvironmentFunctionEntry, Pine.Value ) )
    -> List String
reportEmittedDeclarationsForErrorMsg emittedDeclarations =
    List.map
        (\( functionName, ( FirCompiler.EnvironmentFunctionEntry funcParamCount funcExpectedEnv, _ ) ) ->
            functionName
                ++ " ("
                ++ String.fromInt funcParamCount
                ++ ") env = "
                ++ (case funcExpectedEnv of
                        FirCompiler.LocalEnvironment localEnvExpectedDecls ->
                            "local: "
                                ++ String.fromInt (List.length localEnvExpectedDecls)
                                ++ " ("
                                ++ String.join
                                    ", "
                                    (List.map (\( moduleName, declName ) -> String.join "." (List.concat [ moduleName, [ declName ] ]))
                                        localEnvExpectedDecls
                                    )
                                ++ ")"

                        FirCompiler.ImportedEnvironment _ ->
                            "imported"

                        FirCompiler.IndependentEnvironment ->
                            "independent"
                   )
        )
        emittedDeclarations


compileElmChoiceTypeTagConstructor : ( String, Int ) -> List Expression -> Expression
compileElmChoiceTypeTagConstructor ( tagName, argumentsCount ) arguments =
    if List.length arguments == argumentsCount then
        let
            tagNameAsValue =
                Pine.valueFromString tagName
        in
        inlineElmSyntaxValueConstructor
            tagNameAsValue
            arguments

    else
        let
            ( _, genericContructorValue ) =
                compileElmChoiceTypeTagConstructorValue ( tagName, argumentsCount )
        in
        applicableDeclarationFromConstructorExpression
            (LiteralExpression genericContructorValue)
            arguments


applicableDeclarationFromConstructorExpression : Expression -> (List Expression -> Expression)
applicableDeclarationFromConstructorExpression genericContructorExpression =
    \arguments ->
        case arguments of
            [] ->
                genericContructorExpression

            _ ->
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


compileElmChoiceTypeTagConstructorValue : ( String, Int ) -> ( String, Pine.Value )
compileElmChoiceTypeTagConstructorValue ( tagName, argumentsCount ) =
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
                        [ Pine.ListExpression
                            [ Pine.LiteralExpression Pine.stringAsValue_Literal
                            , Pine.LiteralExpression (Pine.ListValue [ Pine.valueFromString tagName ])
                            ]
                        , Pine.ListExpression
                            [ Pine.LiteralExpression Pine.stringAsValue_List
                            , Pine.ListExpression
                                [ Pine.ListExpression
                                    [ Pine.ListExpression
                                        [ Pine.LiteralExpression Pine.stringAsValue_Literal
                                        , Pine.ListExpression [ Pine.environmentExpr ]
                                        ]
                                    , Pine.environmentExpr
                                        |> Pine.encodeExpressionAsValue
                                        |> Pine.LiteralExpression
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                |> Pine.encodeExpressionAsValue

        _ ->
            case
                evaluateAsIndependentExpression
                    (emitWrapperForPartialApplication (Pine.ListExpression [])
                        argumentsCount
                        (Pine.ListExpression
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
                        )
                    )
            of
                Err _ ->
                    Pine.valueFromString "Failed to compile choice type tag constructor"

                Ok wrappedForExpose ->
                    wrappedForExpose
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
            let
                recordFieldsOrdered : List ( String, Pine.Value, Expression )
                recordFieldsOrdered =
                    List.sortBy
                        (\( fieldName, _, _ ) -> fieldName)
                        (List.map2
                            (\( fieldName, fieldNameValue ) argument ->
                                ( fieldName, fieldNameValue, argument )
                            )
                            recordFieldNamesStringAndValue
                            arguments
                        )
            in
            ListExpression
                [ LiteralExpression elmRecordTypeTagNameAsValue
                , ListExpression
                    [ ListExpression
                        (List.map
                            (\( _, fieldNameValue, argument ) ->
                                ListExpression
                                    [ LiteralExpression fieldNameValue
                                    , argument
                                    ]
                            )
                            recordFieldsOrdered
                        )
                    ]
                ]

        else
            let
                recordFieldsOrdered : List ( String, Pine.Value )
                recordFieldsOrdered =
                    List.sortBy Tuple.first recordFieldNamesStringAndValue
            in
            FunctionApplicationExpression
                (FunctionExpression
                    (List.map (\( fieldName, _ ) -> [ ( fieldName, [] ) ])
                        recordFieldNamesStringAndValue
                    )
                    (ListExpression
                        [ LiteralExpression elmRecordTypeTagNameAsValue
                        , ListExpression
                            [ ListExpression
                                (List.map
                                    (\( fieldName, fieldNameValue ) ->
                                        ListExpression
                                            [ LiteralExpression fieldNameValue
                                            , ReferenceExpression [] fieldName
                                            ]
                                    )
                                    recordFieldsOrdered
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
        {-
           let
               log =
                   Debug.log "shouldInlineDeclaration"
                       { name = name
                       , estimatedSize =
                           case expression of
                               LiteralExpression value ->
                                   String.fromInt (estimatePineValueSize value)

                               _ ->
                                   "not a literal"
                       }
           in
        -}
        case expression of
            LiteralExpression value ->
                {-
                   estimatePineValueSize value < 50 * 1000
                -}
                {-
                   TODO: Instead of going just by the size here, more appropriate would be to
                   check the impact of inlining. For example, we discovered that `Rope.prependTo`
                   (<https://github.com/stil4m/elm-syntax/blob/fcac5e99c1fab7cffea48e728d7034bad526c1e1/src/Rope.elm#L46-L47>)
                   was inlined with an estimated size of 22279, and that inlining led to an exponential growth in
                   expression sizes, because the left size argument was multiplied each time.
                -}
                estimatePineValueSize value < 15 * 1000

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

    else
        case currentDependencies of
            [] ->
                Ok [ currentName ]

            _ ->
                case
                    Common.resultListMapCombine
                        (\currentDependency ->
                            case
                                Common.listFind
                                    (\candidate ->
                                        let
                                            (Elm.Syntax.Node.Node _ candidateModuleDefinition) =
                                                candidate.moduleDefinition
                                        in
                                        Elm.Syntax.Module.moduleName candidateModuleDefinition == currentDependency
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
                        currentDependencies
                of
                    Err ( moduleNames, err ) ->
                        Err ( currentName :: moduleNames, err )

                    Ok ok ->
                        Ok (Common.listUnique (List.concat [ List.concat ok, [ currentName ] ]))


getDirectDependenciesFromModule : Elm.Syntax.File.File -> List Elm.Syntax.ModuleName.ModuleName
getDirectDependenciesFromModule file =
    let
        explicit =
            List.map
                (\(Elm.Syntax.Node.Node _ importSyntax) ->
                    Elm.Syntax.Node.value importSyntax.moduleName
                )
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
    Common.listUnique
        (List.concat [ implicit, explicit ])


valueFromString : String -> Pine.Value
valueFromString string =
    Pine.ListValue
        [ elmStringTypeTagNameAsValue
        , Pine.ListValue [ Pine.valueFromString string ]
        ]


valueFromFloat : Float -> Pine.Value
valueFromFloat float =
    let
        ( numerator, denominator ) =
            searchRatioForFloat float
    in
    if denominator == 1 then
        Pine.valueFromInt numerator

    else
        Pine.ListValue
            [ Pine.valueFromString elmFloatTypeTagName
            , Pine.ListValue [ Pine.valueFromInt numerator, Pine.valueFromInt denominator ]
            ]


searchRatioForFloat : Float -> ( Int, Int )
searchRatioForFloat float =
    if float < 0 then
        let
            ( numeratorAbs, denom ) =
                searchRatioForPositiveFloat 1 -float
        in
        ( -numeratorAbs, denom )

    else
        searchRatioForPositiveFloat 1 float


searchRatioForPositiveFloat : Int -> Float -> ( Int, Int )
searchRatioForPositiveFloat denom float =
    let
        prod =
            toFloat denom * float
    in
    if toFloat (floor prod) == prod then
        ( floor prod, denom )

    else
        searchRatioForPositiveFloat (denom + 1) float


separateEnvironmentDeclarations :
    List ( String, Pine.Value )
    ->
        Result
            String
            { modules : Dict.Dict Elm.Syntax.ModuleName.ModuleName ( Pine.Value, ElmModuleInCompilation )
            , otherDeclarations : List ( String, Pine.Value )
            }
separateEnvironmentDeclarations environmentDeclarations =
    List.foldl
        (\( declNameFlat, declValue ) aggregateResult ->
            case aggregateResult of
                Err err ->
                    Err err

                Ok aggregate ->
                    if stringStartsWithUpper declNameFlat then
                        case getDeclarationsFromEnvironment declValue of
                            Err err ->
                                Err ("Failed get decls from env: " ++ err)

                            Ok ( _, declsFromEnv ) ->
                                case parseModuleValue declsFromEnv of
                                    Err err ->
                                        Err ("Failed to parse module " ++ declNameFlat ++ ": " ++ err)

                                    Ok moduleDeclarations ->
                                        Ok
                                            { modules =
                                                Dict.insert
                                                    (String.split "." declNameFlat)
                                                    ( declValue, moduleDeclarations )
                                                    aggregate.modules
                                            , otherDeclarations = aggregate.otherDeclarations
                                            }

                    else
                        Ok
                            { modules = aggregate.modules
                            , otherDeclarations =
                                ( declNameFlat, declValue ) :: aggregate.otherDeclarations
                            }
        )
        (Ok { modules = Dict.empty, otherDeclarations = [] })
        environmentDeclarations


getDeclarationsFromEnvironment : Pine.Value -> Result String ( List Pine.Value, List ( String, Pine.Value ) )
getDeclarationsFromEnvironment environment =
    case environment of
        Pine.BlobValue _ ->
            Err "Is not a list but a blob"

        Pine.ListValue environmentList ->
            case
                Common.resultListMapCombine
                    (\environmentEntry ->
                        case environmentEntry of
                            Pine.BlobValue _ ->
                                Err "Failed parse env entry: Is not a list but a blob"

                            Pine.ListValue [ nameValue, namedValue ] ->
                                case Pine.stringFromValue nameValue of
                                    Err err ->
                                        Err ("Failed parse env entry: Failed to parse name string: " ++ err)

                                    Ok name ->
                                        Ok ( name, namedValue )

                            Pine.ListValue list ->
                                Err
                                    ("Failed parse env entry: Unexpected number of elements in environment entry list: Not 2 but "
                                        ++ String.fromInt (List.length list)
                                    )
                    )
                    environmentList
            of
                Err err ->
                    Err err

                Ok declarations ->
                    Ok
                        ( environmentList
                        , -- Elm Interactive allows shadowing, so ordering matters here.
                          List.reverse declarations
                        )


{-| Reverses the encoding implemented in emitModuleValue, parsing the Elm module from the transportable form.
-}
parseModuleValue : List ( String, Pine.Value ) -> Result String ElmModuleInCompilation
parseModuleValue moduleValues =
    case
        List.foldr
            (\( declName, declValue ) aggregateResult ->
                case aggregateResult of
                    Err err ->
                        Err err

                    Ok ( functionDeclarations, typeDeclarations ) ->
                        if stringStartsWithUpper declName then
                            case parseTypeDeclarationFromValueTagged declValue of
                                Err err ->
                                    Err err

                                Ok typeDeclaration ->
                                    Ok
                                        ( functionDeclarations
                                        , ( declName, typeDeclaration ) :: typeDeclarations
                                        )

                        else
                            Ok
                                ( ( declName, declValue ) :: functionDeclarations
                                , typeDeclarations
                                )
            )
            (Ok ( [], [] ))
            moduleValues
    of
        Err err ->
            Err err

        Ok ( functionDeclarations, typeDeclarations ) ->
            Ok
                { functionDeclarations = functionDeclarations
                , typeDeclarations = typeDeclarations
                }


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
                                    case parseChoiceTypeFromValue functionRecord of
                                        Err err ->
                                            Err ("Failed to parse choice type: " ++ err)

                                        Ok choiceType ->
                                            Ok (ElmModuleChoiceTypeDeclaration choiceType)

                                "RecordConstructor" ->
                                    case parseRecordConstructorFromValue functionRecord of
                                        Err err ->
                                            Err ("Failed to parse record constructor: " ++ err)

                                        Ok recordConstructor ->
                                            Ok (ElmModuleRecordTypeDeclaration recordConstructor)

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
                                case Pine.stringFromValue tagNameValue of
                                    Err err ->
                                        Err ("Failed to parse string: " ++ err)

                                    Ok tagName ->
                                        case Pine.intFromValue argumentCountValue of
                                            Err err ->
                                                Err ("Failed to parse int: " ++ err)

                                            Ok argumentsCount ->
                                                Ok
                                                    ( tagName
                                                    , ElmModuleChoiceTypeTag argumentsCount
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
                    Ok (ElmModuleChoiceType tags)

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


compileAndEvalParsedInteractiveSubmission : Pine.Value -> InteractiveSubmission -> Result String Pine.Value
compileAndEvalParsedInteractiveSubmission environment submission =
    case compileParsedInteractiveSubmission environment submission of
        Err err ->
            Err ("Failed compile: " ++ err)

        Ok expression ->
            case Pine.evaluateExpression (Pine.EvalEnvironment environment) expression of
                Err err ->
                    Err ("Failed eval: " ++ Pine.displayStringFromPineError err)

                Ok val ->
                    Ok val


compileParsedInteractiveSubmission : Pine.Value -> InteractiveSubmission -> Result String Pine.Expression
compileParsedInteractiveSubmission environment submission =
    case getDeclarationsFromEnvironment environment of
        Err err ->
            Err ("Failed to get declarations from environment: " ++ err)

        Ok ( _, declsFromEnv ) ->
            case separateEnvironmentDeclarations declsFromEnv of
                Err error ->
                    Err ("Failed to separate declarations from environment: " ++ error)

                Ok environmentDeclarations ->
                    let
                        ( defaultCompilationStack, emitStack ) =
                            compilationAndEmitStackFromInteractiveEnvironment
                                { modules =
                                    Dict.map (\_ ( _, parsedModule ) -> parsedModule)
                                        environmentDeclarations.modules
                                , otherDeclarations = environmentDeclarations.otherDeclarations
                                }
                    in
                    case submission of
                        DeclarationSubmission elmDeclaration ->
                            case elmDeclaration of
                                Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                    let
                                        (Elm.Syntax.Node.Node _ declarationName) =
                                            (Elm.Syntax.Node.value functionDeclaration.declaration).name

                                        compilationStack =
                                            { defaultCompilationStack
                                                | inlineableDeclarations =
                                                    List.filter
                                                        (\( inlineableName, _ ) -> inlineableName /= declarationName)
                                                        defaultCompilationStack.inlineableDeclarations
                                            }
                                    in
                                    case
                                        compileElmSyntaxFunction compilationStack functionDeclaration
                                    of
                                        Err err ->
                                            Err ("Failed to compile Elm function declaration: " ++ err)

                                        Ok ( _, functionDeclarationCompilation ) ->
                                            case
                                                FirCompiler.emitExpressionInDeclarationBlock
                                                    emitStack
                                                    [ ( declarationName, functionDeclarationCompilation ) ]
                                                    functionDeclarationCompilation
                                            of
                                                Err err ->
                                                    Err err

                                                Ok declExpression ->
                                                    case FirCompiler.evaluateAsIndependentExpression declExpression of
                                                        Err err ->
                                                            Err ("Failed to compile Elm function declaration: " ++ err)

                                                        Ok declValue ->
                                                            Ok
                                                                (buildExpressionForNewStateAndResponse
                                                                    { newStateExpression =
                                                                        Pine.KernelApplicationExpression
                                                                            "concat"
                                                                            (Pine.ListExpression
                                                                                [ Pine.ListExpression
                                                                                    [ Pine.LiteralExpression
                                                                                        (Pine.valueFromContextExpansionWithName
                                                                                            ( declarationName
                                                                                            , declValue
                                                                                            )
                                                                                        )
                                                                                    ]
                                                                                , Pine.EnvironmentExpression
                                                                                ]
                                                                            )
                                                                    , responseExpression =
                                                                        responseExpressionFromString ("Declared '" ++ declarationName ++ "'")
                                                                    }
                                                                )

                                Elm.Syntax.Declaration.AliasDeclaration _ ->
                                    Err "Alias declaration as submission is not implemented"

                                Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                                    Err "Choice type declaration as submission is not implemented"

                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                    Err "Port declaration as submission is not implemented"

                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                    Err "Infix declaration as submission is not implemented"

                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                    Err "Destructuring as submission is not implemented"

                        ExpressionSubmission elmExpression ->
                            case
                                case compileElmSyntaxExpression defaultCompilationStack elmExpression of
                                    Err err ->
                                        Err err

                                    Ok compiledExpr ->
                                        FirCompiler.emitExpressionInDeclarationBlock
                                            emitStack
                                            []
                                            compiledExpr
                            of
                                Err error ->
                                    Err ("Failed to compile Elm to Pine expression: " ++ error)

                                Ok pineExpression ->
                                    Ok
                                        (buildExpressionForNewStateAndResponse
                                            { newStateExpression = Pine.environmentExpr
                                            , responseExpression = pineExpression
                                            }
                                        )


compilationAndEmitStackFromInteractiveEnvironment :
    { modules : Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    , otherDeclarations : List ( String, Pine.Value )
    }
    -> ( CompilationStack, EmitStack )
compilationAndEmitStackFromInteractiveEnvironment environmentDeclarations =
    let
        interactiveImplicitImportStatements =
            environmentDeclarations.modules
                |> Dict.keys
                |> List.map
                    (\moduleName ->
                        { canonicalModuleName = moduleName
                        , localModuleName = moduleName
                        , exposingList = Nothing
                        }
                    )

        ( defaultCompilationStack, emitStack ) =
            compilationAndEmitStackFromModulesInCompilation
                environmentDeclarations.modules
                { moduleAliases = Dict.empty
                , parsedImports = interactiveImplicitImportStatements
                , localTypeDeclarations = []
                , selfModuleName = []
                }

        compilationStack =
            { defaultCompilationStack
                | inlineableDeclarations =
                    List.concat
                        [ defaultCompilationStack.inlineableDeclarations
                        , List.map
                            (\( declName, declValue ) ->
                                ( declName
                                , applicableDeclarationFromConstructorExpression
                                    (LiteralExpression declValue)
                                )
                            )
                            environmentDeclarations.otherDeclarations
                        ]
            }
    in
    ( compilationStack
    , emitStack
    )


buildExpressionForNewStateAndResponse :
    { newStateExpression : Pine.Expression, responseExpression : Pine.Expression }
    -> Pine.Expression
buildExpressionForNewStateAndResponse config =
    Pine.ListExpression
        [ config.newStateExpression
        , config.responseExpression
        ]


responseExpressionFromString : String -> Pine.Expression
responseExpressionFromString str =
    Pine.LiteralExpression
        (Pine.ListValue
            [ Pine.valueFromString "String"
            , Pine.ListValue [ Pine.valueFromString str ]
            ]
        )
