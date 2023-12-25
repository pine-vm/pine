module FirCompiler exposing (..)

import BigInt
import Dict
import Pine
import Result.Extra
import Set


type Expression
    = LiteralExpression Pine.Value
    | ListExpression (List Expression)
    | KernelApplicationExpression KernelApplicationExpressionStructure
    | ConditionalExpression ConditionalExpressionStructure
    | ReferenceExpression String
    | FunctionExpression (List FunctionParam) Expression
      {-
         Keeping a specialized function application model enables distinguishing cases with immediate full application.
         The emission of specialized code for these cases reduces runtime expenses.
      -}
    | FunctionApplicationExpression Expression (List Expression)
    | LetBlockExpression LetBlockStruct
    | PineFunctionApplicationExpression Pine.Value Expression
      -- The tag expression case is only a wrapper to label a node for inspection and does not influence the evaluation result.
    | StringTagExpression String Expression


type alias KernelApplicationExpressionStructure =
    { functionName : String
    , argument : Expression
    }


type alias ConditionalExpressionStructure =
    { condition : Expression
    , ifTrue : Expression
    , ifFalse : Expression
    }


type alias LetBlockStruct =
    { declarations : List ( String, Expression )
    , expression : Expression
    }


type alias FunctionParam =
    List ( String, List Deconstruction )


type Deconstruction
    = ListItemDeconstruction Int
    | SkipItemsDeconstruction Int
    | PineFunctionApplicationDeconstruction Pine.Value


type alias EmitStack =
    { moduleImports : ModuleImports
    , declarationsDependencies : Dict.Dict String (Set.Set String)

    -- The functions in the first element of the environment list
    , environmentFunctions : List EnvironmentFunctionEntry

    -- Deconstructions we can derive from the second element of the environment list
    , environmentDeconstructions : Dict.Dict String EnvironmentDeconstructionEntry
    }


type alias ModuleImports =
    { importedModules : Dict.Dict (List String) ElmModuleInCompilation
    , importedDeclarations : Dict.Dict String Pine.Value
    }


type alias EnvironmentFunctionEntry =
    { functionName : String
    , argumentsCount : Int
    }


type alias EnvironmentDeconstructionEntry =
    List Deconstruction


type alias ElmModuleInCompilation =
    { declarations : Dict.Dict String Pine.Value
    , choiceTypes : Dict.Dict String ElmModuleChoiceType
    }


type alias ElmModuleChoiceType =
    { tagsNames : Set.Set String
    }


emitExpression : EmitStack -> Expression -> Result String Pine.Expression
emitExpression stack expression =
    case expression of
        LiteralExpression literal ->
            Ok (Pine.LiteralExpression literal)

        ListExpression list ->
            list
                |> List.map (emitExpression stack)
                |> Result.Extra.combine
                |> Result.map Pine.ListExpression
                |> Result.map reduceExpressionToLiteralIfIndependent

        KernelApplicationExpression kernelApplication ->
            kernelApplication.argument
                |> emitExpression stack
                |> Result.map
                    (\argument ->
                        Pine.KernelApplicationExpression
                            { functionName = kernelApplication.functionName
                            , argument = argument
                            }
                    )

        ConditionalExpression conditional ->
            conditional.condition
                |> emitExpression stack
                |> Result.andThen
                    (\condition ->
                        conditional.ifTrue
                            |> emitExpression stack
                            |> Result.andThen
                                (\ifTrue ->
                                    conditional.ifFalse
                                        |> emitExpression stack
                                        |> Result.map
                                            (\ifFalse ->
                                                Pine.ConditionalExpression
                                                    { condition = condition
                                                    , ifTrue = ifTrue
                                                    , ifFalse = ifFalse
                                                    }
                                            )
                                )
                    )

        ReferenceExpression localReference ->
            emitReferenceExpression localReference stack

        FunctionExpression functionParams functionBody ->
            emitFunctionExpression stack functionParams functionBody

        FunctionApplicationExpression functionExpression arguments ->
            emitFunctionApplicationExpression functionExpression arguments stack

        LetBlockExpression letBlock ->
            emitLetBlock stack letBlock

        StringTagExpression tag tagged ->
            tagged
                |> emitExpression stack
                |> Result.map (Pine.StringTagExpression tag)

        PineFunctionApplicationExpression pineFunctionValue argument ->
            emitExpression stack argument
                |> Result.map
                    (\emittedArgument ->
                        Pine.DecodeAndEvaluateExpression
                            { expression = Pine.LiteralExpression pineFunctionValue
                            , environment = emittedArgument
                            }
                    )


emitFunctionExpression : EmitStack -> List FunctionParam -> Expression -> Result String Pine.Expression
emitFunctionExpression stack functionParams functionBody =
    emitExpressionInDeclarationBlock
        stack
        Dict.empty
        (FunctionExpression functionParams functionBody)


emitLetBlock : EmitStack -> LetBlockStruct -> Result String Pine.Expression
emitLetBlock stackBefore letBlock =
    emitExpressionInDeclarationBlock
        stackBefore
        (Dict.fromList letBlock.declarations)
        letBlock.expression


emitExpressionInDeclarationBlock :
    EmitStack
    -> Dict.Dict String Expression
    -> Expression
    -> Result String Pine.Expression
emitExpressionInDeclarationBlock stack environmentDeclarations =
    emitExpressionInDeclarationBlockLessClosure
        stack
        environmentDeclarations
        >> Result.andThen
            (\emitInClosureResult ->
                case emitInClosureResult.closureCaptures of
                    Nothing ->
                        Ok emitInClosureResult.expr

                    Just closureCaptures ->
                        { closureCaptures = closureCaptures }
                            |> emitClosureArgument stack
                            |> Result.mapError ((++) "Failed to emit closure argument for declaration block: ")
                            |> Result.map
                                (\closureArgumentPine ->
                                    partialApplicationExpressionFromListOfArguments
                                        [ closureArgumentPine ]
                                        emitInClosureResult.expr
                                )
            )


emitClosureArgument : EmitStack -> { closureCaptures : List String } -> Result String Pine.Expression
emitClosureArgument stack { closureCaptures } =
    closureCaptures
        |> List.map (emitReferenceExpression >> (|>) stack)
        |> Result.Extra.combine
        |> Result.map Pine.ListExpression


type alias ClosureFunctionEntry =
    { parameters : List FunctionParam
    , innerExpression : Expression
    , closureCaptures : Maybe (List String)
    }


emitExpressionInDeclarationBlockLessClosure :
    EmitStack
    -> Dict.Dict String Expression
    -> Expression
    -> Result String { expr : Pine.Expression, closureCaptures : Maybe (List String) }
emitExpressionInDeclarationBlockLessClosure stackBeforeAddingDeps originalBlockDeclarations originalMainExpression =
    let
        blockDeclarations =
            originalBlockDeclarations
                |> Dict.map
                    (\declarationName declarationExpression ->
                        declarationExpression
                            |> mapLocalDeclarationNamesInDescendants Set.empty
                                ((++) >> (|>) ("____lifted_from_" ++ declarationName))
                    )

        importedModulesDeclarationsFlat : Dict.Dict String Expression
        importedModulesDeclarationsFlat =
            stackBeforeAddingDeps.moduleImports.importedModules
                |> Dict.toList
                |> List.concatMap
                    (\( moduleName, importedModule ) ->
                        importedModule.declarations
                            |> Dict.toList
                            |> List.map
                                (\( declName, declValue ) ->
                                    ( String.join "." (moduleName ++ [ declName ])
                                    , LiteralExpression declValue
                                    )
                                )
                    )
                |> Dict.fromList

        importedDeclarations : Dict.Dict String Expression
        importedDeclarations =
            stackBeforeAddingDeps.moduleImports.importedDeclarations
                |> Dict.map (always LiteralExpression)

        environmentDeclarationsIncludingImports =
            importedModulesDeclarationsFlat
                |> Dict.union importedDeclarations
                |> Dict.union blockDeclarations

        newReferencesDependencies =
            blockDeclarations
                |> Dict.map (always (listDependenciesOfExpression stackBeforeAddingDeps))

        stackWithEnvironmentDeclDeps =
            { stackBeforeAddingDeps
                | declarationsDependencies =
                    Dict.union newReferencesDependencies stackBeforeAddingDeps.declarationsDependencies
            }

        originalMainExpressionDependencies =
            listDependenciesOfExpression stackWithEnvironmentDeclDeps originalMainExpression

        closureCaptures =
            environmentDeclarationsIncludingImports
                |> Dict.keys
                |> List.foldl Set.remove originalMainExpressionDependencies
                |> Set.toList

        blockDeclarationsDirectDependencies =
            blockDeclarations
                |> Dict.map (always (listDependenciesOfExpression stackWithEnvironmentDeclDeps))

        stackBefore =
            { stackWithEnvironmentDeclDeps
                | declarationsDependencies =
                    Dict.union blockDeclarationsDirectDependencies stackWithEnvironmentDeclDeps.declarationsDependencies
            }

        ( stackInClosure, mainExpressionInClosure, closureCapturesReturned ) =
            if closureCaptures == [] then
                ( stackBefore
                , originalMainExpression
                , Nothing
                )

            else
                let
                    closureFunctionParameters =
                        closureCaptures
                            |> List.map (Tuple.pair >> (|>) [] >> List.singleton)

                    closureFunctionParameter =
                        closureParameterFromParameters closureFunctionParameters

                    functionParams =
                        [ closureFunctionParameter ]
                in
                ( { stackBefore
                    | environmentDeconstructions =
                        functionParams
                            |> environmentDeconstructionsFromFunctionParams
                  }
                , FunctionExpression [ closureFunctionParameter ] originalMainExpression
                , Just closureCaptures
                )

        preprocessExpression expression =
            let
                ( functionParameters, functionInnerExpr ) =
                    parseFunctionParameters expression

                ( liftedDeclarationsBeforeParsingFun, expressionAfterLiftingDecls ) =
                    liftDeclsFromLetBlocksRecursively functionInnerExpr
            in
            { functionParameters = functionParameters
            , liftedDeclarations =
                liftedDeclarationsBeforeParsingFun
                    |> List.map (Tuple.mapSecond parseFunctionParameters)
            , expressionAfterLiftingDecls = expressionAfterLiftingDecls
            }

        mainExpressionDependenciesBeforeLift : Set.Set String
        mainExpressionDependenciesBeforeLift =
            listDependenciesOfExpression stackInClosure mainExpressionInClosure

        usedEnvironmentDeclarations =
            environmentDeclarationsIncludingImports
                |> Dict.filter (Set.member >> (|>) mainExpressionDependenciesBeforeLift >> always)
                |> Dict.map (always preprocessExpression)

        envLiftedDeclarationsAsFunctions : Dict.Dict String (List ( String, ClosureFunctionEntry ))
        envLiftedDeclarationsAsFunctions =
            usedEnvironmentDeclarations
                |> Dict.map
                    (\_ envDeclaration ->
                        let
                            closureParam =
                                closureParameterFromParameters envDeclaration.functionParameters
                        in
                        envDeclaration.liftedDeclarations
                            |> List.map
                                (\( envDeclLiftedDeclName, ( envDeclLiftedDeclParams, envDeclLiftedDeclInnerExpr ) ) ->
                                    ( envDeclLiftedDeclName
                                    , { closureCaptures =
                                            closureParam
                                                |> List.map Tuple.first
                                                |> Just
                                      , parameters = closureParam :: envDeclLiftedDeclParams
                                      , innerExpression = envDeclLiftedDeclInnerExpr
                                      }
                                    )
                                )
                    )

        environmentDeclarationsAsFunctionsCommon : List ( String, ClosureFunctionEntry )
        environmentDeclarationsAsFunctionsCommon =
            (usedEnvironmentDeclarations
                |> Dict.map
                    (\_ envDeclaration ->
                        { closureCaptures = Nothing
                        , parameters = envDeclaration.functionParameters
                        , innerExpression = envDeclaration.expressionAfterLiftingDecls
                        }
                    )
                |> Dict.toList
            )
                ++ List.concat (Dict.values envLiftedDeclarationsAsFunctions)

        emitFunction :
            List ( String, ClosureFunctionEntry )
            -> ClosureFunctionEntry
            -> Result String Pine.Expression
        emitFunction environmentDeclarationsAsFunctionsAdditional closureFunctionEntry =
            let
                environmentDeclarationsAsFunctions =
                    environmentDeclarationsAsFunctionsCommon ++ environmentDeclarationsAsFunctionsAdditional

                liftedDeclarationsClosureCaptures =
                    environmentDeclarationsAsFunctions
                        |> List.filterMap
                            (\( functionName, functionEntry ) ->
                                functionEntry.closureCaptures
                                    |> Maybe.map (Tuple.pair functionName)
                            )
                        |> Dict.fromList

                environmentFunctions : List EnvironmentFunctionEntry
                environmentFunctions =
                    environmentDeclarationsAsFunctions
                        |> List.map
                            (\( functionName, functionEntry ) ->
                                { functionName = functionName
                                , argumentsCount = List.length functionEntry.parameters
                                }
                            )

                emitStack =
                    { moduleImports = stackInClosure.moduleImports
                    , declarationsDependencies = stackInClosure.declarationsDependencies
                    , environmentFunctions = environmentFunctions
                    , environmentDeconstructions =
                        closureFunctionEntry.parameters
                            |> environmentDeconstructionsFromFunctionParams
                    }
            in
            closureFunctionEntry.innerExpression
                |> mapReferencesForClosureCaptures liftedDeclarationsClosureCaptures
                |> closurizeFunctionExpressions emitStack Dict.empty
                |> emitExpression emitStack

        emitEnvironmentDeclarationsResult : Result String (List ( String, Pine.Expression ))
        emitEnvironmentDeclarationsResult =
            environmentDeclarationsAsFunctionsCommon
                |> List.map
                    (\( functionName, envDeclAsFunction ) ->
                        envDeclAsFunction
                            |> emitFunction []
                            |> Result.mapError ((++) ("Failed to emit '" ++ functionName ++ "': "))
                            |> Result.map (Tuple.pair functionName)
                    )
                |> Result.Extra.combine
    in
    emitEnvironmentDeclarationsResult
        |> Result.andThen
            (\emitEnvironmentDeclarations ->
                let
                    mainExpressionAfterLift :
                        { functionParameters : List FunctionParam
                        , liftedDeclarations : List ( String, ( List FunctionParam, Expression ) )
                        , expressionAfterLiftingDecls : Expression
                        }
                    mainExpressionAfterLift =
                        mainExpressionInClosure
                            |> mapLocalDeclarationNamesInDescendants Set.empty
                                ((++) >> (|>) "____lifted_from_main")
                            |> preprocessExpression

                    mainExpressionDependenciesAfterLift : Set.Set String
                    mainExpressionDependenciesAfterLift =
                        listDependenciesOfExpression stackInClosure mainExpressionAfterLift.expressionAfterLiftingDecls

                    commonDeclsUsedFromMain =
                        envLiftedDeclarationsAsFunctions
                            |> Dict.values
                            |> List.concat
                            |> List.map Tuple.first
                            |> Set.fromList
                            |> Set.union mainExpressionDependenciesAfterLift
                            |> Set.union mainExpressionDependenciesBeforeLift

                    envFunctionsValuesCommon =
                        emitEnvironmentDeclarations
                            {- The emitted code references environment functions based on their offset in the list.
                               Therefore we maintain the order for all instances reusing the compiled environment functions.
                            -}
                            |> List.map
                                (\( declName, declExpr ) ->
                                    ( declName
                                    , if Set.member declName commonDeclsUsedFromMain then
                                        Pine.encodeExpressionAsValue declExpr

                                      else
                                        Pine.valueFromString "unused-function"
                                    )
                                )

                    functionParametersFromLifted : List FunctionParam
                    functionParametersFromLifted =
                        mainExpressionAfterLift.liftedDeclarations
                            |> List.map (Tuple.mapSecond (always []) >> List.singleton)

                    commonClosureParameter =
                        mainExpressionAfterLift.functionParameters
                            ++ functionParametersFromLifted
                            |> List.concatMap (List.map Tuple.first)
                            |> Set.fromList
                            |> Set.toList
                            |> List.map (Tuple.pair >> (|>) [] >> List.singleton)
                            |> closureParameterFromParameters

                    mainExpressionLiftedDeclarations : List ( String, ClosureFunctionEntry )
                    mainExpressionLiftedDeclarations =
                        mainExpressionAfterLift.liftedDeclarations
                            |> List.map
                                (\( liftedDeclName, ( liftedDeclParams, liftedDeclExpr ) ) ->
                                    ( liftedDeclName
                                    , { parameters = commonClosureParameter :: liftedDeclParams
                                      , innerExpression = liftedDeclExpr
                                      , closureCaptures =
                                            commonClosureParameter
                                                |> List.map Tuple.first
                                                |> Just
                                      }
                                    )
                                )
                in
                mainExpressionLiftedDeclarations
                    |> List.map
                        (\( declName, liftedFromMain ) ->
                            liftedFromMain
                                |> emitFunction mainExpressionLiftedDeclarations
                                |> Result.map (Tuple.pair declName)
                                |> Result.mapError
                                    (\err -> "Failed to emit lifted declaration '" ++ declName ++ "': " ++ err)
                        )
                    |> Result.Extra.combine
                    |> Result.andThen
                        (\liftedFromMainEmitted ->
                            { parameters = mainExpressionAfterLift.functionParameters
                            , innerExpression = mainExpressionAfterLift.expressionAfterLiftingDecls
                            , closureCaptures =
                                commonClosureParameter
                                    |> List.map Tuple.first
                                    |> Just
                            }
                                |> emitFunction mainExpressionLiftedDeclarations
                                |> Result.map
                                    (let
                                        envFunctionsValuesLiftedFromMain =
                                            liftedFromMainEmitted
                                                |> List.map (Tuple.mapSecond Pine.encodeExpressionAsValue)

                                        envFunctionsValues =
                                            envFunctionsValuesCommon ++ envFunctionsValuesLiftedFromMain
                                     in
                                     emitWrapperForPartialApplication
                                        (List.map Tuple.second envFunctionsValues)
                                        (List.length mainExpressionAfterLift.functionParameters)
                                    )
                        )
            )
        |> Result.map
            (\expr ->
                { expr = expr
                , closureCaptures = closureCapturesReturned
                }
            )


parseFunctionParameters : Expression -> ( List FunctionParam, Expression )
parseFunctionParameters expression =
    case expression of
        FunctionExpression functionParams functionBody ->
            let
                ( innerParams, innerBody ) =
                    parseFunctionParameters functionBody
            in
            ( functionParams ++ innerParams, innerBody )

        StringTagExpression _ tagged ->
            parseFunctionParameters tagged

        _ ->
            ( [], expression )


emitReferenceExpression : String -> EmitStack -> Result String Pine.Expression
emitReferenceExpression name compilation =
    case
        compilation.environmentFunctions
            |> List.indexedMap Tuple.pair
            |> List.filter (Tuple.second >> .functionName >> (==) name)
            |> List.head
    of
        Just ( functionIndexInEnv, function ) ->
            emitApplyFunctionFromCurrentEnvironment
                { functionIndexInEnv = functionIndexInEnv
                , function = function
                }
                []
                |> Ok

        Nothing ->
            case Dict.get name compilation.environmentDeconstructions of
                Nothing ->
                    Err
                        ("Failed referencing '"
                            ++ name
                            ++ "'. "
                            ++ String.fromInt (Dict.size compilation.environmentDeconstructions)
                            ++ " deconstructions in scope: "
                            ++ String.join ", " (Dict.keys compilation.environmentDeconstructions)
                            ++ ". "
                            ++ String.fromInt (List.length compilation.environmentFunctions)
                            ++ " functions in scope: "
                            ++ String.join ", " (List.map .functionName compilation.environmentFunctions)
                        )

                Just deconstruction ->
                    Pine.EnvironmentExpression
                        |> listItemFromIndexExpression_Pine 1
                        |> pineExpressionForDeconstructions deconstruction
                        |> Ok


listDependenciesOfExpression : EmitStack -> Expression -> Set.Set String
listDependenciesOfExpression dependenciesRelations expression =
    (case expression of
        LiteralExpression _ ->
            Set.empty

        ListExpression list ->
            list
                |> List.map (listDependenciesOfExpression dependenciesRelations)
                |> List.foldl Set.union Set.empty

        KernelApplicationExpression application ->
            listDependenciesOfExpression dependenciesRelations application.argument

        ConditionalExpression conditional ->
            [ conditional.condition, conditional.ifTrue, conditional.ifFalse ]
                |> listDependenciesOfExpressions dependenciesRelations

        ReferenceExpression reference ->
            Set.singleton reference

        FunctionExpression functionParam functionBody ->
            let
                expressionDependencies =
                    listDependenciesOfExpression dependenciesRelations functionBody
            in
            functionParam
                |> List.concatMap (List.map Tuple.first)
                |> List.foldl Set.remove expressionDependencies

        FunctionApplicationExpression functionExpression arguments ->
            functionExpression
                :: arguments
                |> listDependenciesOfExpressions dependenciesRelations

        LetBlockExpression letBlock ->
            let
                innerDependencies =
                    letBlock.expression
                        :: List.map Tuple.second letBlock.declarations
                        |> listDependenciesOfExpressions dependenciesRelations
            in
            letBlock.declarations
                |> List.map Tuple.first
                |> List.foldl Set.remove innerDependencies

        StringTagExpression _ tagged ->
            listDependenciesOfExpression dependenciesRelations tagged

        PineFunctionApplicationExpression _ argument ->
            argument
                |> listDependenciesOfExpression dependenciesRelations
    )
        |> getTransitiveDependenciesStep dependenciesRelations.declarationsDependencies


getTransitiveDependenciesStep : Dict.Dict String (Set.Set String) -> Set.Set String -> Set.Set String
getTransitiveDependenciesStep dependenciesDependencies current =
    current
        |> Set.toList
        |> List.concatMap
            (Dict.get
                >> (|>) dependenciesDependencies
                >> Maybe.withDefault Set.empty
                >> Set.toList
            )
        |> Set.fromList
        |> Set.union current


listDependenciesOfExpressions : EmitStack -> List Expression -> Set.Set String
listDependenciesOfExpressions dependenciesRelations =
    List.map (listDependenciesOfExpression dependenciesRelations) >> List.foldl Set.union Set.empty


pineExpressionForDeconstructions : List Deconstruction -> Pine.Expression -> Pine.Expression
pineExpressionForDeconstructions =
    List.map pineExpressionForDeconstruction
        >> List.foldr (>>) identity


pineExpressionForDeconstruction : Deconstruction -> Pine.Expression -> Pine.Expression
pineExpressionForDeconstruction deconstruction =
    case deconstruction of
        ListItemDeconstruction index ->
            listItemFromIndexExpression_Pine index

        SkipItemsDeconstruction count ->
            listSkipExpression_Pine count

        PineFunctionApplicationDeconstruction pineFunctionValue ->
            \emittedArgument ->
                Pine.DecodeAndEvaluateExpression
                    { expression = Pine.LiteralExpression pineFunctionValue
                    , environment = emittedArgument
                    }


environmentDeconstructionsFromFunctionParams : List FunctionParam -> Dict.Dict String EnvironmentDeconstructionEntry
environmentDeconstructionsFromFunctionParams =
    closureParameterFromParameters
        >> Dict.fromList


mapReferencesForClosureCaptures : Dict.Dict String (List String) -> Expression -> Expression
mapReferencesForClosureCaptures closureCapturesByFunctionName expression =
    case expression of
        LiteralExpression _ ->
            expression

        ListExpression list ->
            ListExpression (List.map (mapReferencesForClosureCaptures closureCapturesByFunctionName) list)

        KernelApplicationExpression kernelApplication ->
            KernelApplicationExpression
                { kernelApplication
                    | argument =
                        mapReferencesForClosureCaptures closureCapturesByFunctionName kernelApplication.argument
                }

        ConditionalExpression conditional ->
            ConditionalExpression
                { condition =
                    mapReferencesForClosureCaptures closureCapturesByFunctionName conditional.condition
                , ifTrue =
                    mapReferencesForClosureCaptures closureCapturesByFunctionName conditional.ifTrue
                , ifFalse =
                    mapReferencesForClosureCaptures closureCapturesByFunctionName conditional.ifFalse
                }

        ReferenceExpression reference ->
            case Dict.get reference closureCapturesByFunctionName of
                Just capturedParameters ->
                    -- Insert first argument
                    FunctionApplicationExpression
                        expression
                        [ capturedParameters
                            |> List.map ReferenceExpression
                            |> ListExpression
                        ]

                Nothing ->
                    expression

        FunctionExpression functionParam functionBody ->
            FunctionExpression
                functionParam
                (mapReferencesForClosureCaptures closureCapturesByFunctionName functionBody)

        FunctionApplicationExpression functionExpression arguments ->
            let
                mappedArguments =
                    List.map (mapReferencesForClosureCaptures closureCapturesByFunctionName) arguments

                continueWithoutClosureForFunction () =
                    let
                        mappedFunctionExpression =
                            mapReferencesForClosureCaptures closureCapturesByFunctionName functionExpression
                    in
                    FunctionApplicationExpression
                        mappedFunctionExpression
                        mappedArguments
            in
            case functionExpression of
                ReferenceExpression functionName ->
                    case Dict.get functionName closureCapturesByFunctionName of
                        Just capturedParameters ->
                            -- Insert first argument
                            FunctionApplicationExpression
                                (ReferenceExpression functionName)
                                ((capturedParameters
                                    |> List.map ReferenceExpression
                                    |> ListExpression
                                 )
                                    :: mappedArguments
                                )

                        Nothing ->
                            continueWithoutClosureForFunction ()

                _ ->
                    continueWithoutClosureForFunction ()

        LetBlockExpression _ ->
            expression

        StringTagExpression tag tagged ->
            StringTagExpression tag (mapReferencesForClosureCaptures closureCapturesByFunctionName tagged)

        PineFunctionApplicationExpression pineFunctionValue arguments ->
            PineFunctionApplicationExpression
                pineFunctionValue
                (mapReferencesForClosureCaptures closureCapturesByFunctionName arguments)


closurizeFunctionExpressions : EmitStack -> Dict.Dict String (List String) -> Expression -> Expression
closurizeFunctionExpressions stack capturesFromFunctionName expression =
    case expression of
        LiteralExpression _ ->
            expression

        ListExpression list ->
            ListExpression
                (List.map (closurizeFunctionExpressions stack capturesFromFunctionName) list)

        KernelApplicationExpression kernelApplication ->
            KernelApplicationExpression
                { kernelApplication
                    | argument =
                        closurizeFunctionExpressions stack
                            capturesFromFunctionName
                            kernelApplication.argument
                }

        ConditionalExpression conditional ->
            ConditionalExpression
                { condition =
                    closurizeFunctionExpressions stack capturesFromFunctionName conditional.condition
                , ifTrue =
                    closurizeFunctionExpressions stack capturesFromFunctionName conditional.ifTrue
                , ifFalse =
                    closurizeFunctionExpressions stack capturesFromFunctionName conditional.ifFalse
                }

        ReferenceExpression name ->
            case Dict.get name capturesFromFunctionName of
                Just closureCapturesForFunction ->
                    FunctionApplicationExpression
                        expression
                        [ closureCapturesForFunction
                            |> List.map ReferenceExpression
                            |> ListExpression
                        ]

                Nothing ->
                    expression

        FunctionExpression functionParams functionBody ->
            let
                outerDependencies =
                    listDependenciesOfExpression stack expression

                closureCapturesForFunction =
                    outerDependencies
                        |> Set.toList

                closureFunctionParameters =
                    closureCapturesForFunction
                        |> List.map (Tuple.pair >> (|>) [] >> List.singleton)

                closureFunctionParameter =
                    closureParameterFromParameters closureFunctionParameters

                functionBodyMapped =
                    functionBody |> closurizeFunctionExpressions stack capturesFromFunctionName
            in
            if closureCapturesForFunction == [] then
                FunctionExpression functionParams functionBodyMapped

            else
                {-
                   Since we are processing all functions that appear as declaration in a let block to bind free variables,
                   this branch should only be hit for anonymous functions.
                -}
                FunctionApplicationExpression
                    (FunctionExpression
                        (closureFunctionParameter :: functionParams)
                        functionBodyMapped
                    )
                    [ closureCapturesForFunction
                        |> List.map ReferenceExpression
                        |> ListExpression
                    ]

        FunctionApplicationExpression functionExpression arguments ->
            let
                mappedArguments =
                    List.map (closurizeFunctionExpressions stack capturesFromFunctionName) arguments

                mappedFunctionExpression =
                    closurizeFunctionExpressions stack capturesFromFunctionName functionExpression

                ( mappedArgumentsMerged, mappedFunctionExpressionMerged ) =
                    case mappedFunctionExpression of
                        FunctionApplicationExpression innerFunction innerArguments ->
                            ( innerArguments ++ mappedArguments
                            , innerFunction
                            )

                        _ ->
                            ( mappedArguments
                            , mappedFunctionExpression
                            )
            in
            FunctionApplicationExpression
                mappedFunctionExpressionMerged
                mappedArgumentsMerged

        LetBlockExpression letBlock ->
            let
                processLetDeclaration : Expression -> Maybe ( List String, Expression )
                processLetDeclaration declExpression =
                    case declExpression of
                        FunctionExpression functionParams functionBody ->
                            let
                                outerDependencies =
                                    listDependenciesOfExpression stack declExpression

                                closureCapturesForFunction =
                                    outerDependencies
                                        |> Set.toList

                                closureFunctionParameters =
                                    closureCapturesForFunction
                                        |> List.map (Tuple.pair >> (|>) [] >> List.singleton)

                                closureFunctionParameter =
                                    closureParameterFromParameters closureFunctionParameters
                            in
                            if closureCapturesForFunction == [] then
                                Nothing

                            else
                                Just
                                    ( closureCapturesForFunction
                                    , FunctionExpression
                                        (closureFunctionParameter :: functionParams)
                                        functionBody
                                    )

                        _ ->
                            Nothing

                declarationsBeforeMappingApplications : List ( String, ( Dict.Dict String (List String), Expression ) )
                declarationsBeforeMappingApplications =
                    letBlock.declarations
                        |> List.map
                            (\( declName, declExpression ) ->
                                ( declName
                                , processLetDeclaration declExpression
                                    |> Maybe.map (Tuple.mapFirst (Dict.singleton declName))
                                    |> Maybe.withDefault ( Dict.empty, declExpression )
                                )
                            )

                newClosureInstructions =
                    declarationsBeforeMappingApplications
                        |> List.map (Tuple.second >> Tuple.first)
                        |> List.foldl Dict.union Dict.empty

                closurizeFunctionExpressionsInner =
                    closurizeFunctionExpressions
                        stack
                        (Dict.union capturesFromFunctionName newClosureInstructions)
            in
            LetBlockExpression
                { letBlock
                    | declarations =
                        declarationsBeforeMappingApplications
                            |> List.map (Tuple.mapSecond Tuple.second)
                            |> List.map (Tuple.mapSecond closurizeFunctionExpressionsInner)
                    , expression =
                        closurizeFunctionExpressionsInner letBlock.expression
                }

        StringTagExpression tag tagged ->
            StringTagExpression tag (closurizeFunctionExpressions stack capturesFromFunctionName tagged)

        PineFunctionApplicationExpression pineFunctionValue argument ->
            PineFunctionApplicationExpression
                pineFunctionValue
                (closurizeFunctionExpressions stack capturesFromFunctionName argument)


closureParameterFromParameters : List FunctionParam -> FunctionParam
closureParameterFromParameters =
    List.indexedMap
        (\paramIndex ->
            List.map (Tuple.mapSecond ((::) (ListItemDeconstruction paramIndex)))
        )
        >> List.concat


mapLocalDeclarationNamesInDescendants : Set.Set String -> (String -> String) -> Expression -> Expression
mapLocalDeclarationNamesInDescendants localSet mapDeclarationName expression =
    case expression of
        LiteralExpression _ ->
            expression

        ListExpression list ->
            ListExpression (List.map (mapLocalDeclarationNamesInDescendants localSet mapDeclarationName) list)

        KernelApplicationExpression kernelApplication ->
            KernelApplicationExpression
                { kernelApplication
                    | argument =
                        mapLocalDeclarationNamesInDescendants localSet mapDeclarationName kernelApplication.argument
                }

        ConditionalExpression conditional ->
            ConditionalExpression
                { condition =
                    mapLocalDeclarationNamesInDescendants localSet mapDeclarationName conditional.condition
                , ifTrue =
                    mapLocalDeclarationNamesInDescendants localSet mapDeclarationName conditional.ifTrue
                , ifFalse =
                    mapLocalDeclarationNamesInDescendants localSet mapDeclarationName conditional.ifFalse
                }

        ReferenceExpression reference ->
            if Set.member reference localSet then
                ReferenceExpression (mapDeclarationName reference)

            else
                expression

        FunctionExpression functionParams functionBody ->
            let
                localSetWithParameters =
                    functionParams
                        |> List.concatMap (List.map Tuple.first)
                        |> List.foldl
                            Set.insert
                            localSet

                mappedParameters =
                    functionParams
                        |> List.map (List.map (Tuple.mapFirst mapDeclarationName))
            in
            FunctionExpression
                mappedParameters
                (mapLocalDeclarationNamesInDescendants
                    localSetWithParameters
                    mapDeclarationName
                    functionBody
                )

        FunctionApplicationExpression functionExpression arguments ->
            FunctionApplicationExpression
                (mapLocalDeclarationNamesInDescendants localSet mapDeclarationName functionExpression)
                (List.map (mapLocalDeclarationNamesInDescendants localSet mapDeclarationName) arguments)

        LetBlockExpression letBlock ->
            let
                localSetWithDeclarations =
                    List.foldl
                        (Tuple.first >> Set.insert)
                        localSet
                        letBlock.declarations

                mappedDeclarations =
                    List.map
                        (Tuple.mapFirst mapDeclarationName
                            >> Tuple.mapSecond
                                (mapLocalDeclarationNamesInDescendants localSetWithDeclarations mapDeclarationName)
                        )
                        letBlock.declarations
            in
            LetBlockExpression
                { declarations = mappedDeclarations
                , expression =
                    mapLocalDeclarationNamesInDescendants
                        localSetWithDeclarations
                        mapDeclarationName
                        letBlock.expression
                }

        StringTagExpression tag tagged ->
            StringTagExpression
                tag
                (mapLocalDeclarationNamesInDescendants localSet mapDeclarationName tagged)

        PineFunctionApplicationExpression pineFunctionValue argument ->
            PineFunctionApplicationExpression
                pineFunctionValue
                (mapLocalDeclarationNamesInDescendants localSet mapDeclarationName argument)


liftDeclsFromLetBlocksRecursively : Expression -> ( List ( String, Expression ), Expression )
liftDeclsFromLetBlocksRecursively expression =
    case expression of
        LiteralExpression _ ->
            ( [], expression )

        ListExpression list ->
            let
                elements =
                    List.map liftDeclsFromLetBlocksRecursively list
            in
            ( List.concatMap Tuple.first elements
            , ListExpression (List.map Tuple.second elements)
            )

        KernelApplicationExpression kernelApplication ->
            kernelApplication.argument
                |> liftDeclsFromLetBlocksRecursively
                |> Tuple.mapSecond
                    (\argument ->
                        KernelApplicationExpression { kernelApplication | argument = argument }
                    )

        ConditionalExpression conditional ->
            let
                ( conditionDeclarations, conditionExpression ) =
                    liftDeclsFromLetBlocksRecursively conditional.condition

                ( ifTrueDeclarations, ifTrueExpression ) =
                    liftDeclsFromLetBlocksRecursively conditional.ifTrue

                ( ifFalseDeclarations, ifFalseExpression ) =
                    liftDeclsFromLetBlocksRecursively conditional.ifFalse
            in
            ( conditionDeclarations ++ ifTrueDeclarations ++ ifFalseDeclarations
            , ConditionalExpression
                { condition = conditionExpression
                , ifTrue = ifTrueExpression
                , ifFalse = ifFalseExpression
                }
            )

        ReferenceExpression name ->
            ( []
            , ReferenceExpression name
            )

        FunctionExpression _ _ ->
            ( [], expression )

        FunctionApplicationExpression function arguments ->
            let
                ( argumentsDeclarations, argumentsExpressions ) =
                    arguments
                        |> List.map liftDeclsFromLetBlocksRecursively
                        |> List.unzip

                ( functionDeclarations, functionExpression ) =
                    function
                        |> liftDeclsFromLetBlocksRecursively
            in
            ( List.concat argumentsDeclarations ++ functionDeclarations
            , FunctionApplicationExpression
                functionExpression
                argumentsExpressions
            )

        LetBlockExpression letBlock ->
            let
                ( innerDecls, mappedExpression ) =
                    liftDeclsFromLetBlocksRecursively letBlock.expression
            in
            ( letBlock.declarations ++ innerDecls
            , mappedExpression
            )

        StringTagExpression tag tagged ->
            tagged
                |> liftDeclsFromLetBlocksRecursively
                |> Tuple.mapSecond (StringTagExpression tag)

        PineFunctionApplicationExpression pineFunctionValue argument ->
            liftDeclsFromLetBlocksRecursively argument
                |> Tuple.mapSecond (PineFunctionApplicationExpression pineFunctionValue)


emitFunctionApplicationExpression : Expression -> List Expression -> EmitStack -> Result String Pine.Expression
emitFunctionApplicationExpression functionExpression arguments compilation =
    arguments
        |> List.indexedMap
            (\argumentIndex ->
                emitExpression compilation
                    >> Result.mapError
                        ((++)
                            ("Failed emitting argument "
                                ++ String.fromInt argumentIndex
                                ++ " for function application: "
                            )
                        )
            )
        |> Result.Extra.combine
        |> Result.andThen
            (\argumentsPine ->
                let
                    genericFunctionApplication () =
                        emitExpression compilation functionExpression
                            |> Result.mapError ((++) "Failed emitting function expression: ")
                            |> Result.andThen (emitFunctionApplicationExpressionPine argumentsPine)
                in
                case functionExpression of
                    ReferenceExpression functionName ->
                        case
                            compilation.environmentFunctions
                                |> List.indexedMap Tuple.pair
                                |> List.filter (Tuple.second >> .functionName >> (==) functionName)
                                |> List.head
                        of
                            Just ( functionIndexInEnv, function ) ->
                                emitApplyFunctionFromCurrentEnvironment
                                    { functionIndexInEnv = functionIndexInEnv
                                    , function = function
                                    }
                                    argumentsPine
                                    |> Ok

                            Nothing ->
                                genericFunctionApplication ()

                    _ ->
                        genericFunctionApplication ()
            )


emitFunctionApplicationExpressionPine : List Pine.Expression -> Pine.Expression -> Result String Pine.Expression
emitFunctionApplicationExpressionPine arguments functionExpressionPine =
    let
        genericPartialApplication () =
            partialApplicationExpressionFromListOfArguments arguments
                functionExpressionPine
    in
    if not (pineExpressionIsIndependent functionExpressionPine) then
        genericPartialApplication ()
            |> Ok

    else
        evaluateAsIndependentExpression functionExpressionPine
            |> Result.map
                (\functionValue ->
                    case parseFunctionRecordFromValueTagged functionValue of
                        Err _ ->
                            genericPartialApplication ()

                        Ok functionRecord ->
                            let
                                combinedArguments =
                                    [ List.map Pine.LiteralExpression
                                        functionRecord.argumentsAlreadyCollected
                                    , arguments
                                    ]
                                        |> List.concat
                            in
                            if functionRecord.functionParameterCount /= List.length combinedArguments then
                                genericPartialApplication ()

                            else
                                let
                                    mappedEnvironment =
                                        Pine.ListExpression
                                            [ functionRecord.envFunctions
                                                |> List.map Pine.LiteralExpression
                                                |> Pine.ListExpression
                                            , Pine.ListExpression combinedArguments
                                            ]

                                    findReplacementForExpression expression =
                                        if expression == Pine.EnvironmentExpression then
                                            Just mappedEnvironment

                                        else
                                            Nothing
                                in
                                transformPineExpressionWithOptionalReplacement
                                    findReplacementForExpression
                                    functionRecord.innerFunction
                                    |> Tuple.first
                                    |> searchForExpressionReductionRecursive { maxDepth = 5 }
                )


emitApplyFunctionFromCurrentEnvironment :
    { functionIndexInEnv : Int
    , function : EnvironmentFunctionEntry
    }
    -> List Pine.Expression
    -> Pine.Expression
emitApplyFunctionFromCurrentEnvironment { functionIndexInEnv, function } arguments =
    let
        getEnvFunctionsExpression =
            Pine.EnvironmentExpression
                |> listItemFromIndexExpression_Pine 0

        getFunctionExpression =
            getEnvFunctionsExpression
                |> listItemFromIndexExpression_Pine functionIndexInEnv
    in
    if function.argumentsCount == List.length arguments then
        Pine.DecodeAndEvaluateExpression
            { expression = getFunctionExpression
            , environment =
                Pine.ListExpression
                    [ getEnvFunctionsExpression
                    , Pine.ListExpression arguments
                    ]
            }

    else
        (if function.argumentsCount == 0 then
            emitWrapperForPartialApplicationZero
                { getFunctionInnerExpression = getFunctionExpression
                , getEnvFunctionsExpression = getEnvFunctionsExpression
                }

         else
            buildRecordOfPartiallyAppliedFunction
                { getFunctionInnerExpression = getFunctionExpression
                , getEnvFunctionsExpression = getEnvFunctionsExpression
                , functionParameterCount = function.argumentsCount
                , argumentsAlreadyCollected = []
                }
        )
            |> partialApplicationExpressionFromListOfArguments arguments


partialApplicationExpressionFromListOfArguments : List Pine.Expression -> Pine.Expression -> Pine.Expression
partialApplicationExpressionFromListOfArguments arguments function =
    case arguments of
        [] ->
            function

        nextArgument :: followingArguments ->
            partialApplicationExpressionFromListOfArguments
                followingArguments
                (adaptivePartialApplicationExpression
                    { function = function
                    , argument = nextArgument
                    }
                )


emitWrapperForPartialApplication : List Pine.Value -> Int -> Pine.Expression -> Pine.Expression
emitWrapperForPartialApplication envFunctions parameterCount innerExpression =
    if parameterCount == 0 then
        emitWrapperForPartialApplicationZero
            { getFunctionInnerExpression =
                innerExpression
                    |> Pine.encodeExpressionAsValue
                    |> Pine.LiteralExpression
            , getEnvFunctionsExpression =
                Pine.LiteralExpression (Pine.ListValue envFunctions)
            }

    else
        buildRecordOfPartiallyAppliedFunction
            { getFunctionInnerExpression =
                innerExpression
                    |> Pine.encodeExpressionAsValue
                    |> Pine.LiteralExpression
            , functionParameterCount = parameterCount
            , getEnvFunctionsExpression =
                Pine.LiteralExpression (Pine.ListValue envFunctions)
            , argumentsAlreadyCollected = []
            }


emitWrapperForPartialApplicationZero :
    { getFunctionInnerExpression : Pine.Expression
    , getEnvFunctionsExpression : Pine.Expression
    }
    -> Pine.Expression
emitWrapperForPartialApplicationZero { getFunctionInnerExpression, getEnvFunctionsExpression } =
    Pine.DecodeAndEvaluateExpression
        { expression = getFunctionInnerExpression
        , environment =
            Pine.ListExpression
                [ getEnvFunctionsExpression
                , Pine.ListExpression []
                ]
        }


adaptivePartialApplicationExpression : { function : Pine.Expression, argument : Pine.Expression } -> Pine.Expression
adaptivePartialApplicationExpression { function, argument } =
    Pine.DecodeAndEvaluateExpression
        { expression =
            adaptivePartialApplicationExpressionStaticPart
                |> Pine.encodeExpressionAsValue
                |> Pine.LiteralExpression
        , environment =
            Pine.ListExpression
                [ function
                , argument
                ]
        }


adaptivePartialApplicationExpressionStaticPart : Pine.Expression
adaptivePartialApplicationExpressionStaticPart =
    let
        functionExpression =
            listItemFromIndexExpression_Pine 0 Pine.EnvironmentExpression

        newArgumentExpression =
            listItemFromIndexExpression_Pine 1 Pine.EnvironmentExpression
    in
    Pine.ConditionalExpression
        { condition =
            {-
               If the first element in 'function' equals 'Function',
            -}
            equalCondition_Pine
                [ listItemFromIndexExpression_Pine 0 functionExpression
                , Pine.LiteralExpression (Pine.valueFromString "Function")
                ]
        , ifTrue =
            {-
               assume the second list item is a list with the following items:
               + 0: inner function
               + 1: number of parameters expected by the inner function
               + 2: captured environment functions
               + 3: the arguments collected so far.
            -}
            let
                partiallyAppliedFunctionRecord =
                    listItemFromIndexExpression_Pine 1 functionExpression

                innerFunction =
                    partiallyAppliedFunctionRecord
                        |> listItemFromIndexExpression_Pine 0

                numberOfParametersExpectedByInnerFunction =
                    partiallyAppliedFunctionRecord
                        |> listItemFromIndexExpression_Pine 1

                environmentFunctions =
                    partiallyAppliedFunctionRecord
                        |> listItemFromIndexExpression_Pine 2

                previouslyCollectedArguments =
                    partiallyAppliedFunctionRecord
                        |> listItemFromIndexExpression_Pine 3

                collectedArguments =
                    Pine.KernelApplicationExpression
                        { functionName = "concat"
                        , argument =
                            Pine.ListExpression
                                [ previouslyCollectedArguments
                                , Pine.ListExpression [ newArgumentExpression ]
                                ]
                        }

                collectedArgumentsLength =
                    countListElementsExpression_Pine collectedArguments

                collectedArgumentsAreComplete =
                    equalCondition_Pine
                        [ collectedArgumentsLength
                        , numberOfParametersExpectedByInnerFunction
                        ]
            in
            -- First, check if the argument we collect here is the last one.
            Pine.ConditionalExpression
                { condition = collectedArgumentsAreComplete
                , ifTrue =
                    -- If it is, we can apply the inner function.
                    Pine.DecodeAndEvaluateExpression
                        { expression = innerFunction
                        , environment =
                            Pine.ListExpression
                                [ environmentFunctions
                                , collectedArguments
                                ]
                        }
                , ifFalse =
                    -- If it is not, we need to collect more arguments.
                    updateRecordOfPartiallyAppliedFunction
                        { getFunctionInnerExpression = innerFunction
                        , functionParameterCountExpression = numberOfParametersExpectedByInnerFunction
                        , getEnvFunctionsExpression = environmentFunctions
                        , argumentsAlreadyCollectedExpression = collectedArguments
                        }
                }
        , ifFalse =
            attemptReduceDecodeAndEvaluateExpressionRecursiveWithDefaultDepth
                { expression = functionExpression
                , environment = newArgumentExpression
                }
        }


buildRecordOfPartiallyAppliedFunction :
    { getFunctionInnerExpression : Pine.Expression
    , getEnvFunctionsExpression : Pine.Expression
    , functionParameterCount : Int
    , argumentsAlreadyCollected : List Pine.Expression
    }
    -> Pine.Expression
buildRecordOfPartiallyAppliedFunction config =
    updateRecordOfPartiallyAppliedFunction
        { getFunctionInnerExpression = config.getFunctionInnerExpression
        , getEnvFunctionsExpression = config.getEnvFunctionsExpression
        , functionParameterCountExpression =
            Pine.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt config.functionParameterCount))
        , argumentsAlreadyCollectedExpression = Pine.ListExpression config.argumentsAlreadyCollected
        }


updateRecordOfPartiallyAppliedFunction :
    { getFunctionInnerExpression : Pine.Expression
    , getEnvFunctionsExpression : Pine.Expression
    , functionParameterCountExpression : Pine.Expression
    , argumentsAlreadyCollectedExpression : Pine.Expression
    }
    -> Pine.Expression
updateRecordOfPartiallyAppliedFunction config =
    Pine.ListExpression
        [ Pine.LiteralExpression (Pine.valueFromString "Function")
        , Pine.ListExpression
            [ config.getFunctionInnerExpression
            , config.functionParameterCountExpression
            , config.getEnvFunctionsExpression
            , config.argumentsAlreadyCollectedExpression
            ]
        ]


parseFunctionRecordFromValueTagged :
    Pine.Value
    ->
        Result
            String
            { innerFunction : Pine.Expression
            , functionParameterCount : Int
            , envFunctions : List Pine.Value
            , argumentsAlreadyCollected : List Pine.Value
            }
parseFunctionRecordFromValueTagged value =
    case value of
        Pine.BlobValue _ ->
            Err "Is not a list but a blob"

        Pine.ListValue listItems ->
            case listItems of
                [ functionTag, functionRecord ] ->
                    if functionTag == Pine.valueFromString "Function" then
                        parseFunctionRecordFromValue functionRecord

                    else
                        Err "Is not tagged as 'Function'"

                _ ->
                    Err
                        ("List does not have the expected number of items: "
                            ++ String.fromInt (List.length listItems)
                        )


parseFunctionRecordFromValue :
    Pine.Value
    ->
        Result
            String
            { innerFunction : Pine.Expression
            , functionParameterCount : Int
            , envFunctions : List Pine.Value
            , argumentsAlreadyCollected : List Pine.Value
            }
parseFunctionRecordFromValue value =
    case value of
        Pine.ListValue listItems ->
            case listItems of
                [ innerFunctionValue, functionParameterCountValue, envFunctionsValue, argumentsAlreadyCollectedValue ] ->
                    case Pine.decodeExpressionFromValue innerFunctionValue of
                        Err err ->
                            Err ("Failed to decode inner function: " ++ err)

                        Ok innerFunction ->
                            case
                                functionParameterCountValue
                                    |> Pine.bigIntFromValue
                                    |> Result.andThen
                                        (BigInt.toString
                                            >> String.toInt
                                            >> Result.fromMaybe "Failed to map from BigInt to Int"
                                        )
                            of
                                Err err ->
                                    Err ("Failed to decode function parameter count: " ++ err)

                                Ok functionParameterCount ->
                                    case envFunctionsValue of
                                        Pine.ListValue envFunctions ->
                                            case argumentsAlreadyCollectedValue of
                                                Pine.ListValue argumentsAlreadyCollected ->
                                                    Ok
                                                        { innerFunction = innerFunction
                                                        , functionParameterCount = functionParameterCount
                                                        , envFunctions = envFunctions
                                                        , argumentsAlreadyCollected = argumentsAlreadyCollected
                                                        }

                                                _ ->
                                                    Err "argumentsAlreadyCollectedValue is not a list"

                                        _ ->
                                            Err "envFunctionsValue is not a list"

                _ ->
                    Err
                        ("List does not have the expected number of items: "
                            ++ String.fromInt (List.length listItems)
                        )

        Pine.BlobValue _ ->
            Err "Is not a list but a blob"


attemptReduceDecodeAndEvaluateExpressionRecursiveWithDefaultDepth :
    Pine.DecodeAndEvaluateExpressionStructure
    -> Pine.Expression
attemptReduceDecodeAndEvaluateExpressionRecursiveWithDefaultDepth originalExpression =
    let
        sizeBeforeReduction =
            [ originalExpression.expression, originalExpression.environment ]
                |> List.map (countPineExpressionSize estimatePineValueSize)
                |> List.sum

        reductionMaxDepth =
            if sizeBeforeReduction < 10 * 1000 then
                2

            else
                1
    in
    attemptReduceDecodeAndEvaluateExpressionRecursive
        { maxDepth = reductionMaxDepth }
        originalExpression


attemptReduceDecodeAndEvaluateExpressionRecursive :
    { maxDepth : Int }
    -> Pine.DecodeAndEvaluateExpressionStructure
    -> Pine.Expression
attemptReduceDecodeAndEvaluateExpressionRecursive { maxDepth } originalExpression =
    let
        default =
            Pine.DecodeAndEvaluateExpression originalExpression
    in
    if maxDepth < 1 then
        default

    else
        case searchReductionForDecodeAndEvaluateExpression originalExpression of
            Nothing ->
                default

            Just reduced ->
                case reduced of
                    Pine.DecodeAndEvaluateExpression reducedDecodeAndEval ->
                        attemptReduceDecodeAndEvaluateExpressionRecursive
                            { maxDepth = maxDepth - 1 }
                            reducedDecodeAndEval

                    _ ->
                        reduced


searchReductionForDecodeAndEvaluateExpression :
    Pine.DecodeAndEvaluateExpressionStructure
    -> Maybe Pine.Expression
searchReductionForDecodeAndEvaluateExpression originalExpression =
    if pineExpressionIsIndependent originalExpression.expression then
        case Pine.evaluateExpression Pine.emptyEvalContext originalExpression.expression of
            Err _ ->
                Nothing

            Ok expressionValue ->
                case Pine.decodeExpressionFromValue expressionValue of
                    Err _ ->
                        Nothing

                    Ok decodedExpression ->
                        let
                            findReplacementForExpression expression =
                                if expression == Pine.EnvironmentExpression then
                                    Just originalExpression.environment

                                else
                                    Nothing

                            transformResult =
                                transformPineExpressionWithOptionalReplacement
                                    findReplacementForExpression
                                    decodedExpression
                        in
                        if (Tuple.second transformResult).referencesOriginalEnvironment then
                            Nothing

                        else
                            let
                                reducedExpression =
                                    transformResult
                                        |> Tuple.first
                                        |> searchForExpressionReductionRecursive { maxDepth = 5 }
                            in
                            Just reducedExpression

    else
        Nothing


searchForExpressionReductionRecursive : { maxDepth : Int } -> Pine.Expression -> Pine.Expression
searchForExpressionReductionRecursive { maxDepth } expression =
    if maxDepth < 1 then
        expression

    else
        let
            transformed =
                expression
                    |> transformPineExpressionWithOptionalReplacement searchForExpressionReduction
                    |> Tuple.first
        in
        if transformed == expression then
            transformed

        else
            searchForExpressionReductionRecursive { maxDepth = maxDepth - 1 } transformed


reduceExpressionToLiteralIfIndependent : Pine.Expression -> Pine.Expression
reduceExpressionToLiteralIfIndependent expression =
    if pineExpressionIsIndependent expression then
        case Pine.evaluateExpression Pine.emptyEvalContext expression of
            Err _ ->
                expression

            Ok expressionValue ->
                Pine.LiteralExpression expressionValue

    else
        expression


searchForExpressionReduction : Pine.Expression -> Maybe Pine.Expression
searchForExpressionReduction expression =
    let
        attemptReduceViaEval () =
            if pineExpressionIsIndependent expression then
                case Pine.evaluateExpression Pine.emptyEvalContext expression of
                    Err _ ->
                        Nothing

                    Ok expressionValue ->
                        Just (Pine.LiteralExpression expressionValue)

            else
                Nothing
    in
    case expression of
        Pine.LiteralExpression _ ->
            Nothing

        Pine.KernelApplicationExpression rootKernelApp ->
            case rootKernelApp.functionName of
                "list_head" ->
                    case rootKernelApp.argument of
                        Pine.ListExpression argumentList ->
                            List.head argumentList

                        _ ->
                            attemptReduceViaEval ()

                "skip" ->
                    case rootKernelApp.argument of
                        Pine.ListExpression [ Pine.LiteralExpression skipCountLiteral, Pine.ListExpression expressionList ] ->
                            case
                                skipCountLiteral
                                    |> Pine.bigIntFromValue
                                    |> Result.toMaybe
                                    |> Maybe.andThen (BigInt.toString >> String.toInt)
                            of
                                Nothing ->
                                    attemptReduceViaEval ()

                                Just skipCount ->
                                    expressionList
                                        |> List.drop skipCount
                                        |> Pine.ListExpression
                                        |> Just

                        _ ->
                            attemptReduceViaEval ()

                _ ->
                    attemptReduceViaEval ()

        _ ->
            attemptReduceViaEval ()


transformPineExpressionWithOptionalReplacement :
    (Pine.Expression -> Maybe Pine.Expression)
    -> Pine.Expression
    -> ( Pine.Expression, { referencesOriginalEnvironment : Bool } )
transformPineExpressionWithOptionalReplacement findReplacement expression =
    case findReplacement expression of
        Just replacement ->
            ( replacement, { referencesOriginalEnvironment = False } )

        Nothing ->
            case expression of
                Pine.LiteralExpression _ ->
                    ( expression, { referencesOriginalEnvironment = False } )

                Pine.ListExpression list ->
                    let
                        itemsResults =
                            list
                                |> List.map (transformPineExpressionWithOptionalReplacement findReplacement)
                    in
                    ( Pine.ListExpression (List.map Tuple.first itemsResults)
                    , { referencesOriginalEnvironment =
                            itemsResults |> List.any (Tuple.second >> .referencesOriginalEnvironment)
                      }
                    )

                Pine.DecodeAndEvaluateExpression decodeAndEvaluate ->
                    let
                        expressionResult =
                            transformPineExpressionWithOptionalReplacement findReplacement decodeAndEvaluate.expression

                        environmentResult =
                            transformPineExpressionWithOptionalReplacement findReplacement decodeAndEvaluate.environment
                    in
                    ( Pine.DecodeAndEvaluateExpression
                        { expression = Tuple.first expressionResult
                        , environment = Tuple.first environmentResult
                        }
                    , { referencesOriginalEnvironment =
                            (Tuple.second expressionResult).referencesOriginalEnvironment
                                || (Tuple.second environmentResult).referencesOriginalEnvironment
                      }
                    )

                Pine.KernelApplicationExpression kernelApp ->
                    kernelApp.argument
                        |> transformPineExpressionWithOptionalReplacement findReplacement
                        |> Tuple.mapFirst
                            (\argument ->
                                Pine.KernelApplicationExpression { argument = argument, functionName = kernelApp.functionName }
                            )

                Pine.ConditionalExpression conditional ->
                    let
                        condition =
                            transformPineExpressionWithOptionalReplacement findReplacement conditional.condition

                        ifTrue =
                            transformPineExpressionWithOptionalReplacement findReplacement conditional.ifTrue

                        ifFalse =
                            transformPineExpressionWithOptionalReplacement findReplacement conditional.ifFalse
                    in
                    ( Pine.ConditionalExpression
                        { condition = Tuple.first condition
                        , ifTrue = Tuple.first ifTrue
                        , ifFalse = Tuple.first ifFalse
                        }
                    , { referencesOriginalEnvironment =
                            [ condition, ifTrue, ifFalse ]
                                |> List.map (Tuple.second >> .referencesOriginalEnvironment)
                                |> List.any identity
                      }
                    )

                Pine.EnvironmentExpression ->
                    ( Pine.EnvironmentExpression
                    , { referencesOriginalEnvironment = True
                      }
                    )

                Pine.StringTagExpression tag tagged ->
                    tagged
                        |> transformPineExpressionWithOptionalReplacement findReplacement
                        |> Tuple.mapFirst
                            (\taggedMapped ->
                                Pine.StringTagExpression tag taggedMapped
                            )


evaluateAsIndependentExpression : Pine.Expression -> Result String Pine.Value
evaluateAsIndependentExpression expression =
    if not (pineExpressionIsIndependent expression) then
        Err "Expression is not independent"

    else
        Pine.evaluateExpression
            Pine.emptyEvalContext
            expression
            |> Result.mapError
                (Pine.displayStringFromPineError
                    >> (++) "Expression seems independent but failed to evaluate: "
                )


pineExpressionIsIndependent : Pine.Expression -> Bool
pineExpressionIsIndependent expression =
    case expression of
        Pine.LiteralExpression _ ->
            True

        Pine.ListExpression list ->
            List.all pineExpressionIsIndependent list

        Pine.DecodeAndEvaluateExpression decodeAndEval ->
            [ decodeAndEval.environment, decodeAndEval.expression ]
                |> List.all pineExpressionIsIndependent

        Pine.KernelApplicationExpression kernelApp ->
            pineExpressionIsIndependent kernelApp.argument

        Pine.ConditionalExpression conditional ->
            [ conditional.condition, conditional.ifTrue, conditional.ifFalse ]
                |> List.all pineExpressionIsIndependent

        Pine.EnvironmentExpression ->
            False

        Pine.StringTagExpression _ tagged ->
            pineExpressionIsIndependent tagged


listItemFromIndexExpression : Int -> Expression -> Expression
listItemFromIndexExpression itemIndex listExpression =
    pineKernel_ListHead (listSkipExpression itemIndex listExpression)


countListElementsExpression : Expression -> Expression
countListElementsExpression listExpression =
    KernelApplicationExpression
        { functionName = "length"
        , argument = listExpression
        }


pineKernel_ListHead : Expression -> Expression
pineKernel_ListHead listExpression =
    KernelApplicationExpression
        { functionName = "list_head"
        , argument = listExpression
        }


listSkipExpression : Int -> Expression -> Expression
listSkipExpression numberToDrop listExpression =
    if numberToDrop < 1 then
        listExpression

    else
        applyKernelFunctionWithTwoArguments
            "skip"
            (LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt numberToDrop)))
            listExpression


equalCondition : List Expression -> Expression
equalCondition list =
    KernelApplicationExpression
        { functionName = "equal"
        , argument = ListExpression list
        }


applyKernelFunctionWithTwoArguments : String -> Expression -> Expression -> Expression
applyKernelFunctionWithTwoArguments kernelFunctionName argA argB =
    KernelApplicationExpression
        { functionName = kernelFunctionName
        , argument = ListExpression [ argA, argB ]
        }


tagValueExpression : String -> List Expression -> Expression
tagValueExpression tagName tagArgumentsExpressions =
    ListExpression
        [ LiteralExpression (Pine.valueFromString tagName)
        , ListExpression tagArgumentsExpressions
        ]


countListElementsExpression_Pine : Pine.Expression -> Pine.Expression
countListElementsExpression_Pine listExpression =
    Pine.KernelApplicationExpression
        { functionName = "length"
        , argument = listExpression
        }


listItemFromIndexExpression_Pine : Int -> Pine.Expression -> Pine.Expression
listItemFromIndexExpression_Pine itemIndex listExpression =
    pineKernel_ListHead_Pine (listSkipExpression_Pine itemIndex listExpression)


listSkipExpression_Pine : Int -> Pine.Expression -> Pine.Expression
listSkipExpression_Pine numberToDrop listExpression =
    if numberToDrop < 1 then
        listExpression

    else
        applyKernelFunctionWithTwoArguments_Pine
            "skip"
            (Pine.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt numberToDrop)))
            listExpression


pineKernel_ListHead_Pine : Pine.Expression -> Pine.Expression
pineKernel_ListHead_Pine listExpression =
    Pine.KernelApplicationExpression
        { functionName = "list_head"
        , argument = listExpression
        }


equalCondition_Pine : List Pine.Expression -> Pine.Expression
equalCondition_Pine list =
    Pine.KernelApplicationExpression
        { functionName = "equal"
        , argument = Pine.ListExpression list
        }


applyKernelFunctionWithTwoArguments_Pine : String -> Pine.Expression -> Pine.Expression -> Pine.Expression
applyKernelFunctionWithTwoArguments_Pine kernelFunctionName argA argB =
    Pine.KernelApplicationExpression
        { functionName = kernelFunctionName
        , argument = Pine.ListExpression [ argA, argB ]
        }


countPineExpressionSize : (Pine.Value -> Int) -> Pine.Expression -> Int
countPineExpressionSize countValueSize expression =
    case expression of
        Pine.LiteralExpression literal ->
            countValueSize literal

        Pine.ListExpression list ->
            1 + List.sum (List.map (countPineExpressionSize countValueSize) list)

        Pine.DecodeAndEvaluateExpression decodeAndEval ->
            [ decodeAndEval.environment, decodeAndEval.expression ]
                |> List.map (countPineExpressionSize countValueSize)
                |> List.sum

        Pine.KernelApplicationExpression kernelApp ->
            2 + countPineExpressionSize countValueSize kernelApp.argument

        Pine.ConditionalExpression conditional ->
            [ conditional.condition, conditional.ifTrue, conditional.ifFalse ]
                |> List.map (countPineExpressionSize countValueSize)
                |> List.sum

        Pine.EnvironmentExpression ->
            1

        Pine.StringTagExpression _ tagged ->
            countPineExpressionSize countValueSize tagged


estimatePineValueSize : Pine.Value -> Int
estimatePineValueSize value =
    case value of
        Pine.BlobValue blob ->
            10 + List.length blob

        Pine.ListValue list ->
            10 + List.sum (List.map estimatePineValueSize list)
