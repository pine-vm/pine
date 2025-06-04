module FirCompiler exposing
    ( DeclBlockClosureCaptures(..)
    , DeclBlockFunctionEntry(..)
    , DeclBlockRootDeps(..)
    , Deconstruction(..)
    , EmitDeclarationBlockResult(..)
    , EmitStack
    , EnvironmentDeconstructionEntry
    , EnvironmentFunctionEntry(..)
    , Expression(..)
    , FunctionEnvironment(..)
    , ParsedFunctionEnvFunctions(..)
    , ParsedFunctionValue(..)
    , attemptReduceParseAndEvalExpressionRecursive
    , buildRecordOfPartiallyAppliedFunction
    , countListElementsExpression
    , emitDeclarationBlock
    , emitExpression
    , emitExpressionInDeclarationBlock
    , emitWrapperForPartialApplication
    , emitWrapperForPartialApplicationZero
    , equalCondition
    , equalCondition_Pine
    , estimatePineValueSize
    , evaluateAsIndependentExpression
    , getTransitiveDependencies
    , inlineLocalReferences
    , listFunctionAppExpressions
    , listItemFromIndexExpression
    , listItemFromIndexExpression_Pine
    , listSkipExpression
    , listSkipExpression_Pine
    , listUnboundReferencesInExpression
    , parseFunctionParameters
    , parseFunctionRecordFromValueTagged
    , partialApplicationExpressionFromListOfArguments
    , pineExpressionIsIndependent
    , pineKernel_Head
    , pineKernel_Head_Pine
    , recursionDomainsFromDeclarationDependencies
    )

import Common
import Dict
import Pine
import Set


type Expression
    = LiteralExpression Pine.Value
    | ListExpression (List Expression)
    | KernelApplicationExpression String Expression
    | ConditionalExpression Expression Expression Expression
    | FunctionExpression (List FunctionParam) Expression
      {-
         Keeping a specialized function application model enables distinguishing cases with immediate full application.
         The emission of specialized code for these cases reduces runtime expenses.
      -}
    | FunctionApplicationExpression Expression (List Expression)
      {-
         The reference expression references a name introduced by a parent declaration block or function param deconstruction.
         Referencing a declaration from a declaration block enables (mutual) recursion.
         References, in general, enable modeling closures.
      -}
    | ReferenceExpression (List String) String
    | DeclarationBlockExpression (List ( String, Expression )) Expression
    | PineFunctionApplicationExpression Pine.Expression Expression
      -- The tag expression case is only a wrapper to label a node for inspection and does not influence the evaluation result.
    | StringTagExpression String Expression


type alias FunctionParam =
    List ( String, List Deconstruction )


type Deconstruction
    = ListItemDeconstruction Int
    | SkipItemsDeconstruction Int
    | PineFunctionApplicationDeconstruction Pine.Expression


type alias EmitStack =
    { importedFunctions : List ( List String, List ( String, ( EnvironmentFunctionEntry, Pine.Value ) ) )
    , importedFunctionsToInline : List ( List String, List ( String, Pine.Value ) )

    -- The functions in the first item in the environment list
    , environmentFunctions : List ( ( List String, String ), EnvironmentFunctionEntry )

    -- Deconstructions we can derive from the second item in the environment list
    , environmentDeconstructions : List ( String, EnvironmentDeconstructionEntry )
    , skipApplyFunction : Bool
    }


{-| The recursive function implementing adaptive application of function arguments.
-}
environmentFunctionPartialApplicationName : String
environmentFunctionPartialApplicationName =
    "<internal-partial-application>"


type EnvironmentFunctionEntry
    = EnvironmentFunctionEntry Int FunctionEnvironment


type FunctionEnvironment
    = LocalEnvironment
        -- List of expected declarations
        (List ( List String, String ))
    | ImportedEnvironment
        -- Path to the tagged function record relative to the entry in the current environment.
        (List Deconstruction)
    | IndependentEnvironment


type alias EnvironmentDeconstructionEntry =
    List Deconstruction


type DeclBlockClosureCaptures
    = DeclBlockClosureCaptures (List ( String, EnvironmentDeconstructionEntry ))


type DeclBlockRootDeps
    = DeclBlockRootDeps (List Expression)


type ClosureCapture
    = DeconstructionCapture EnvironmentDeconstructionEntry
    | ExpressionCapture Expression


type DeclBlockFunctionEntry
    = DeclBlockFunctionEntry
        -- parameters
        (List FunctionParam)
        -- innerExpression
        Expression


type ParsedFunctionValue
    = ParsedFunctionValue Pine.Value (() -> Result String Pine.Expression) Int ParsedFunctionEnvFunctions (List Pine.Value)


type ParsedFunctionEnvFunctions
    = ParsedFunctionEnvFunctions (List Pine.Value)


type EmitDeclarationBlockResult
    = EmitDeclarationBlockResult
        -- newEnvFunctionsValues
        (List ( String, ( EnvironmentFunctionEntry, ( Pine.Expression, Pine.Value ) ) ))
        -- parseAndEmitFunction
        (Expression -> ( DeclBlockFunctionEntry, Result String Pine.Expression ))
        -- envFunctionsExpression
        Pine.Expression


emitExpression : EmitStack -> Expression -> Result String Pine.Expression
emitExpression stack expression =
    case expression of
        LiteralExpression literal ->
            Ok (Pine.LiteralExpression literal)

        ListExpression list ->
            case Common.resultListMapCombine (\item -> emitExpression stack item) list of
                Err err ->
                    Err err

                Ok listEmitted ->
                    Ok (reduceExpressionToLiteralIfIndependent (Pine.ListExpression listEmitted))

        KernelApplicationExpression functionName inputFir ->
            case emitExpression stack inputFir of
                Err err ->
                    Err err

                Ok input ->
                    Ok (Pine.KernelApplicationExpression functionName input)

        ConditionalExpression conditionFir falseBranchFir trueBranchFir ->
            case emitExpression stack conditionFir of
                Err err ->
                    Err err

                Ok condition ->
                    case emitExpression stack trueBranchFir of
                        Err err ->
                            Err err

                        Ok trueBranch ->
                            case emitExpression stack falseBranchFir of
                                Err err ->
                                    Err err

                                Ok falseBranch ->
                                    Ok
                                        (Pine.ConditionalExpression
                                            condition
                                            falseBranch
                                            trueBranch
                                        )

        ReferenceExpression moduleName localReference ->
            emitReferenceExpression ( moduleName, localReference ) stack

        FunctionExpression functionParams functionBody ->
            emitFunctionExpression stack functionParams functionBody

        FunctionApplicationExpression functionExpression arguments ->
            let
                ( consFunction, consArgs ) =
                    consolidateNestedFunctionApplications functionExpression arguments
            in
            emitFunctionApplication consFunction consArgs stack

        DeclarationBlockExpression declarations innerExpression ->
            emitExpressionInDeclarationBlock
                stack
                declarations
                innerExpression

        StringTagExpression tag tagged ->
            case emitExpression stack tagged of
                Err err ->
                    Err err

                Ok emitted ->
                    Ok (Pine.StringTagExpression tag emitted)

        PineFunctionApplicationExpression pineFunctionExpression argument ->
            case emitExpression stack argument of
                Err err ->
                    Err err

                Ok emittedArgument ->
                    Ok
                        (attemptReduceParseAndEvalExpressionRecursive
                            3
                            ( Pine.LiteralExpression
                                (Pine.encodeExpressionAsValue pineFunctionExpression)
                            , emittedArgument
                            )
                        )


emitFunctionExpression : EmitStack -> List FunctionParam -> Expression -> Result String Pine.Expression
emitFunctionExpression stack functionParams functionBody =
    emitExpressionInDeclarationBlock
        stack
        []
        (FunctionExpression functionParams functionBody)


emitExpressionInDeclarationBlock :
    EmitStack
    -> List ( String, Expression )
    -> Expression
    -> Result String Pine.Expression
emitExpressionInDeclarationBlock stackBefore blockDeclarations mainExpression =
    let
        blockDeclarationsDependencies : List ( String, List String )
        blockDeclarationsDependencies =
            List.map
                (\( declName, declExpression ) ->
                    ( declName
                    , listUnboundReferencesInExpression declExpression []
                    )
                )
                blockDeclarations

        mainExpressionOuterDependencies : List String
        mainExpressionOuterDependencies =
            listTransitiveDependenciesOfExpression
                (Dict.fromList blockDeclarationsDependencies)
                mainExpression

        usedBlockDeclarations : List ( String, Expression )
        usedBlockDeclarations =
            List.filter
                (\( declName, _ ) -> List.member declName mainExpressionOuterDependencies)
                blockDeclarations

        mainExpressionAsFunction : DeclBlockFunctionEntry
        mainExpressionAsFunction =
            parseFunctionParameters mainExpression

        (DeclBlockFunctionEntry mainExprParams mainExprInnerExpr) =
            mainExpressionAsFunction

        closureCaptures : List ( String, EnvironmentDeconstructionEntry )
        closureCaptures =
            List.filter
                (\( declName, _ ) ->
                    List.member declName mainExpressionOuterDependencies
                )
                stackBefore.environmentDeconstructions

        continueEmitBlock () =
            case
                emitDeclarationBlock
                    stackBefore
                    usedBlockDeclarations
                    (DeclBlockClosureCaptures closureCaptures)
                    (DeclBlockRootDeps [ mainExpression ])
            of
                Err err ->
                    Err err

                Ok ( _, EmitDeclarationBlockResult _ parseAndEmitFunction envFunctionsExpression ) ->
                    let
                        ( _, mainExpressionEmitResult ) =
                            parseAndEmitFunction mainExpression
                    in
                    case mainExpressionEmitResult of
                        Err err ->
                            Err ("Failed emitting main expression: " ++ err)

                        Ok mainExpressionEmitted ->
                            Ok
                                (emitWrapperForPartialApplication
                                    envFunctionsExpression
                                    (List.length mainExprParams)
                                    mainExpressionEmitted
                                )
    in
    case ( mainExprParams, usedBlockDeclarations ) of
        ( [], [] ) ->
            let
                mainExpressionImports : List ( List String, String )
                mainExpressionImports =
                    listImportingReferencesInExpression mainExpression

                mainDependsOnImport : Bool
                mainDependsOnImport =
                    List.any
                        (\importedName ->
                            case Common.assocListGet importedName stackBefore.environmentFunctions of
                                Nothing ->
                                    True

                                _ ->
                                    False
                        )
                        mainExpressionImports
            in
            if mainDependsOnImport then
                continueEmitBlock ()

            else
                let
                    needsAdaptiveApplication =
                        case stackBefore.environmentFunctions of
                            [] ->
                                expressionNeedsAdaptiveApplication [] mainExprInnerExpr

                            _ ->
                                {-
                                   If the stackBefore.environmentFunctions is not empty, assume we already added nessecary internals
                                   in a parent scope.
                                -}
                                False
                in
                if needsAdaptiveApplication then
                    continueEmitBlock ()

                else
                    emitExpression stackBefore mainExprInnerExpr

        _ ->
            continueEmitBlock ()


emitDeclarationBlock :
    EmitStack
    -> List ( String, Expression )
    -> DeclBlockClosureCaptures
    -> DeclBlockRootDeps
    -> Result String ( EmitStack, EmitDeclarationBlockResult )
emitDeclarationBlock stackBefore blockDeclarations (DeclBlockClosureCaptures configClosureCaptures) (DeclBlockRootDeps rootDependencies) =
    let
        blockDeclarationsNames : List String
        blockDeclarationsNames =
            List.map Tuple.first blockDeclarations

        availableEmittedDependencies : Dict.Dict String (List String)
        availableEmittedDependencies =
            case Common.assocListGet [] stackBefore.importedFunctions of
                Nothing ->
                    Dict.empty

                Just prevCompiledDecls ->
                    List.foldl
                        (\( functionName, ( EnvironmentFunctionEntry _ expectedEnvironment, _ ) ) aggregate ->
                            case expectedEnvironment of
                                LocalEnvironment localEnvExpectedDecls ->
                                    let
                                        localEnvExpectedDeclsLocal : List String
                                        localEnvExpectedDeclsLocal =
                                            List.concatMap
                                                (\( depModuleName, depDeclName ) ->
                                                    case depModuleName of
                                                        [] ->
                                                            [ depDeclName ]

                                                        _ ->
                                                            []
                                                )
                                                localEnvExpectedDecls
                                    in
                                    case localEnvExpectedDeclsLocal of
                                        [] ->
                                            aggregate

                                        _ ->
                                            Dict.insert
                                                functionName
                                                localEnvExpectedDeclsLocal
                                                aggregate

                                ImportedEnvironment _ ->
                                    aggregate

                                IndependentEnvironment ->
                                    aggregate
                        )
                        Dict.empty
                        prevCompiledDecls

        blockDeclarationsDirectDependencies : List ( String, List String )
        blockDeclarationsDirectDependencies =
            List.map
                (\( declName, declExpression ) ->
                    ( declName
                    , Common.listUnique (listUnboundReferencesInExpression declExpression [])
                    )
                )
                blockDeclarations

        localDependenciesRelations : Dict.Dict String (List String)
        localDependenciesRelations =
            List.foldl
                (\( declName, declDeps ) aggregate ->
                    Dict.insert declName declDeps aggregate
                )
                availableEmittedDependencies
                blockDeclarationsDirectDependencies

        parentDependenciesRel : Dict.Dict String (List String)
        parentDependenciesRel =
            List.foldl
                (\( ( moduleName, localName ), EnvironmentFunctionEntry _ expectedEnv ) aggregate ->
                    if moduleName == [] then
                        case expectedEnv of
                            LocalEnvironment dependenciesNames ->
                                let
                                    dependenciesNamesLocal : List String
                                    dependenciesNamesLocal =
                                        List.filterMap
                                            (\( envModuleName, envLocalName ) ->
                                                if envModuleName == [] then
                                                    Just envLocalName

                                                else
                                                    Nothing
                                            )
                                            dependenciesNames
                                in
                                Dict.insert localName dependenciesNamesLocal aggregate

                            ImportedEnvironment _ ->
                                aggregate

                            IndependentEnvironment ->
                                aggregate

                    else
                        aggregate
                )
                Dict.empty
                stackBefore.environmentFunctions

        dependenciesRelations : Dict.Dict String (List String)
        dependenciesRelations =
            Dict.union
                localDependenciesRelations
                parentDependenciesRel

        referencedImports : List ( ( List String, String ), EnvironmentFunctionEntry, Pine.Expression )
        referencedImports =
            referencedImportsFromRoots
                rootDependencies
                stackBefore
                dependenciesRelations
                blockDeclarations

        ( emittedImports, forwardedImports ) =
            List.foldl
                (\( name, entry, expr ) ( emitted, forwarded ) ->
                    case Common.assocListGet name stackBefore.environmentFunctions of
                        Nothing ->
                            ( ( name, entry, expr ) :: emitted, forwarded )

                        Just _ ->
                            ( emitted, name :: forwarded )
                )
                ( [], [] )
                referencedImports

        blockDeclarationsParsed : List ( String, DeclBlockFunctionEntry )
        blockDeclarationsParsed =
            List.map (Tuple.mapSecond parseFunctionParameters) blockDeclarations

        rootDependenciesNames : List String
        rootDependenciesNames =
            Common.listUnique
                (List.concatMap
                    (\depExpr -> listUnboundReferencesInExpression depExpr [])
                    rootDependencies
                )

        allLocalDependencies : List String
        allLocalDependencies =
            getTransitiveDependencies
                dependenciesRelations
                rootDependenciesNames

        contentsDependOnFunctionApplication : Bool
        contentsDependOnFunctionApplication =
            if
                List.any
                    (\( _, EnvironmentFunctionEntry _ importedFunctionEnv, _ ) ->
                        case importedFunctionEnv of
                            LocalEnvironment localEnv ->
                                List.member ( [], environmentFunctionPartialApplicationName ) localEnv

                            _ ->
                                False
                    )
                    emittedImports
            then
                True

            else if
                List.any
                    (\( _, declExpression ) ->
                        expressionNeedsAdaptiveApplication knownFunctionParamCounts declExpression
                    )
                    blockDeclarations
            then
                True

            else
                List.any
                    (\rootDep -> expressionNeedsAdaptiveApplication knownFunctionParamCounts rootDep)
                    rootDependencies

        shouldForward : ( List String, String ) -> Bool
        shouldForward ( moduleName, localName ) =
            if
                contentsDependOnFunctionApplication
                    && (localName == environmentFunctionPartialApplicationName)
            then
                True

            else if moduleName == [] then
                List.member localName allLocalDependencies

            else
                List.member ( moduleName, localName ) forwardedImports

        forwardedDecls : List ( ( List String, String ), EnvironmentFunctionEntry, Int )
        forwardedDecls =
            List.filter (\( name, _, _ ) -> shouldForward name)
                (List.indexedMap
                    (\beforeIndex ( envFuncName, envFunctionEntry ) ->
                        ( envFuncName, envFunctionEntry, beforeIndex )
                    )
                    stackBefore.environmentFunctions
                )

        knownFunctionParamCounts : List ( ( List String, String ), Int )
        knownFunctionParamCounts =
            List.concat
                [ List.map
                    (\( declName, DeclBlockFunctionEntry declParams _ ) ->
                        ( ( [], declName )
                        , List.length declParams
                        )
                    )
                    blockDeclarationsParsed
                , List.map
                    (\( ( moduleName, declName ), EnvironmentFunctionEntry paramCount _, _ ) ->
                        ( ( moduleName, declName )
                        , paramCount
                        )
                    )
                    emittedImports
                ]

        usedAvailableEmittedForInternals : List ( ( List String, String ), EnvironmentFunctionEntry, Pine.Expression )
        usedAvailableEmittedForInternals =
            case forwardedDecls of
                [] ->
                    if contentsDependOnFunctionApplication then
                        [ ( ( [], environmentFunctionPartialApplicationName )
                          , EnvironmentFunctionEntry 0 IndependentEnvironment
                          , Pine.LiteralExpression (adaptivePartialApplicationRecursiveValue ())
                          )
                        ]

                    else
                        []

                _ ->
                    {-
                       If the stackBefore.environmentFunctions is not empty, assume we already added nessecary internals
                       in a parent scope.
                    -}
                    []

        usedAvailableEmitted : List ( ( List String, String ), EnvironmentFunctionEntry, Pine.Expression )
        usedAvailableEmitted =
            List.concat
                [ usedAvailableEmittedForInternals
                , emittedImports
                ]

        allBlockDeclarationsAsFunctions : List ( String, DeclBlockFunctionEntry )
        allBlockDeclarationsAsFunctions =
            List.map
                (\( declName, declExpression ) ->
                    ( declName
                    , parseFunctionParameters declExpression
                    )
                )
                blockDeclarations

        composeEnvironmentFunctions :
            { prefix : List a
            , forwarded : List a
            , appendedFromDecls : List a
            , appendedFromClosureCaptures : List a
            }
            -> List a
        composeEnvironmentFunctions { prefix, forwarded, appendedFromDecls, appendedFromClosureCaptures } =
            List.concat
                [ prefix, forwarded, appendedFromDecls, appendedFromClosureCaptures ]

        prefixEnvironmentFunctions : List ( ( List String, String ), EnvironmentFunctionEntry )
        prefixEnvironmentFunctions =
            List.map
                (\( functionName, functionEntry, _ ) -> ( functionName, functionEntry ))
                usedAvailableEmitted

        prependedEnvFunctionsExpressions : List Pine.Expression
        prependedEnvFunctionsExpressions =
            List.map
                (\( _, _, emittedExpr ) -> emittedExpr)
                usedAvailableEmitted

        closureCapturesForBlockDecls : List ( String, Expression )
        closureCapturesForBlockDecls =
            {-
               To avoid repeated evaluation of declarations without parameters from a let-block at runtime,
               Map them to closure captures list so these are only evaluated once.
            -}
            List.foldl
                (\( declName, DeclBlockFunctionEntry asFunctionParams asFunctionInnerExpr ) aggregate ->
                    case asFunctionParams of
                        [] ->
                            case Common.assocListGet declName blockDeclarationsDirectDependencies of
                                Nothing ->
                                    aggregate

                                Just declDirectDeps ->
                                    let
                                        declTransitiveDeps =
                                            getTransitiveDependencies
                                                dependenciesRelations
                                                declDirectDeps
                                    in
                                    if
                                        List.any
                                            (\depName -> List.member depName declTransitiveDeps)
                                            blockDeclarationsNames
                                    then
                                        aggregate

                                    else
                                        ( declName, asFunctionInnerExpr ) :: aggregate

                        _ ->
                            -- Do not include functions into closureCapturesForBlockDecls
                            aggregate
                )
                []
                allBlockDeclarationsAsFunctions

        blockDeclarationsAsFunctionsLessClosure : List ( String, DeclBlockFunctionEntry )
        blockDeclarationsAsFunctionsLessClosure =
            List.filter
                (\( declName, _ ) ->
                    case Common.assocListGet declName closureCapturesForBlockDecls of
                        Nothing ->
                            True

                        _ ->
                            False
                )
                allBlockDeclarationsAsFunctions

        closureCaptures : List ( String, ClosureCapture )
        closureCaptures =
            List.concat
                [ List.map (Tuple.mapSecond DeconstructionCapture)
                    configClosureCaptures
                , List.map (Tuple.mapSecond ExpressionCapture)
                    closureCapturesForBlockDecls
                ]

        newEnvironmentFunctionsNames : List ( List String, String )
        newEnvironmentFunctionsNames =
            composeEnvironmentFunctions
                { prefix =
                    List.map
                        (\( functionName, _, _ ) -> functionName)
                        usedAvailableEmitted
                , forwarded =
                    List.map
                        (\( declName, _, _ ) -> declName)
                        forwardedDecls
                , appendedFromDecls =
                    List.map
                        (\( declName, _ ) -> ( [], declName ))
                        blockDeclarationsAsFunctionsLessClosure
                , appendedFromClosureCaptures =
                    List.map
                        (\( declName, _ ) -> ( [], declName ))
                        closureCaptures
                }

        newEnvironmentFunctionsFromDecls : List ( ( List String, String ), EnvironmentFunctionEntry )
        newEnvironmentFunctionsFromDecls =
            List.map
                (\( functionName, DeclBlockFunctionEntry functionEntryParams _ ) ->
                    ( ( [], functionName )
                    , EnvironmentFunctionEntry
                        (List.length functionEntryParams)
                        (LocalEnvironment newEnvironmentFunctionsNames)
                    )
                )
                blockDeclarationsAsFunctionsLessClosure

        newEnvironmentFunctionsFromClosureCaptures : List ( ( List String, String ), EnvironmentFunctionEntry )
        newEnvironmentFunctionsFromClosureCaptures =
            List.map
                (\( captureName, _ ) ->
                    ( ( [], captureName )
                    , EnvironmentFunctionEntry 0 IndependentEnvironment
                    )
                )
                closureCaptures

        environmentFunctions : List ( ( List String, String ), EnvironmentFunctionEntry )
        environmentFunctions =
            composeEnvironmentFunctions
                { prefix = prefixEnvironmentFunctions
                , forwarded =
                    List.map
                        (\( declName, entry, _ ) -> ( declName, entry ))
                        forwardedDecls
                , appendedFromDecls = newEnvironmentFunctionsFromDecls
                , appendedFromClosureCaptures = newEnvironmentFunctionsFromClosureCaptures
                }

        emitFunctionStack : List ( String, EnvironmentDeconstructionEntry ) -> EmitStack
        emitFunctionStack environmentDeconstructions =
            { importedFunctions =
                {-
                   stackBefore.importedFunctions

                    Our approach is to emit all importedFunctions we depend on in the topmost declaration block.
                    Therefore, nested declaration blocks should not depend on importedFunctions.

                -}
                -- []
                stackBefore.importedFunctions
            , importedFunctionsToInline = stackBefore.importedFunctionsToInline
            , environmentFunctions = environmentFunctions
            , environmentDeconstructions = environmentDeconstructions
            , skipApplyFunction = stackBefore.skipApplyFunction
            }

        emitFunction : DeclBlockFunctionEntry -> Result String Pine.Expression
        emitFunction (DeclBlockFunctionEntry functionEntryParams functionEntryInnerExpression) =
            emitExpression
                (emitFunctionStack (closureParameterFromParameters 0 functionEntryParams))
                functionEntryInnerExpression

        emitBlockDeclarationsResult : Result String (List ( String, ( DeclBlockFunctionEntry, Pine.Expression ) ))
        emitBlockDeclarationsResult =
            Common.resultListMapCombine
                (\( functionName, blockDeclAsFunction ) ->
                    case emitFunction blockDeclAsFunction of
                        Err err ->
                            Err ("Failed to emit '" ++ functionName ++ "': " ++ err)

                        Ok emittedExpression ->
                            Ok ( functionName, ( blockDeclAsFunction, emittedExpression ) )
                )
                blockDeclarationsAsFunctionsLessClosure

        closureCapturesExpressionsResult : Result String (List Pine.Expression)
        closureCapturesExpressionsResult =
            Common.resultListMapCombine
                (\( _, closureCapture ) ->
                    case closureCapture of
                        DeconstructionCapture deconstruction ->
                            Ok
                                (pineExpressionForDeconstructions
                                    deconstruction
                                    (listItemFromIndexExpression_Pine 1 Pine.environmentExpr)
                                )

                        ExpressionCapture expression ->
                            emitExpression stackBefore expression
                )
                closureCaptures
    in
    case closureCapturesExpressionsResult of
        Err err ->
            Err err

        Ok closureCapturesExpressions ->
            case emitBlockDeclarationsResult of
                Err err ->
                    Err err

                Ok blockDeclarationsEmitted ->
                    let
                        newEnvFunctionsValues : List ( String, ( EnvironmentFunctionEntry, ( Pine.Expression, Pine.Value ) ) )
                        newEnvFunctionsValues =
                            List.map
                                (\( declName, ( DeclBlockFunctionEntry declAsFunctionParams _, declExpr ) ) ->
                                    ( declName
                                    , ( EnvironmentFunctionEntry
                                            (List.length declAsFunctionParams)
                                            (LocalEnvironment newEnvironmentFunctionsNames)
                                      , ( declExpr
                                        , Pine.encodeExpressionAsValue declExpr
                                        )
                                      )
                                    )
                                )
                                blockDeclarationsEmitted

                        newEnvFunctionsExpressionsFromDecls : List Pine.Expression
                        newEnvFunctionsExpressionsFromDecls =
                            List.map
                                (\( _, ( _, ( _, newFuncValue ) ) ) -> Pine.LiteralExpression newFuncValue)
                                newEnvFunctionsValues

                        appendedEnvFunctionsExpressions : List Pine.Expression
                        appendedEnvFunctionsExpressions =
                            List.concat
                                [ newEnvFunctionsExpressionsFromDecls
                                , closureCapturesExpressions
                                ]

                        prevEnvFunctionsExpr : Pine.Expression
                        prevEnvFunctionsExpr =
                            listItemFromIndexExpression_Pine 0 Pine.environmentExpr

                        forwardedItems : List Pine.Expression
                        forwardedItems =
                            List.map
                                (\( _, _, indexInParentEnv ) ->
                                    listItemFromIndexExpression_Pine indexInParentEnv prevEnvFunctionsExpr
                                )
                                forwardedDecls

                        envFunctionsExpression : Pine.Expression
                        envFunctionsExpression =
                            Pine.ListExpression
                                (List.concat
                                    [ prependedEnvFunctionsExpressions
                                    , forwardedItems
                                    , appendedEnvFunctionsExpressions
                                    ]
                                )

                        parseAndEmitFunction : Expression -> ( DeclBlockFunctionEntry, Result String Pine.Expression )
                        parseAndEmitFunction expression =
                            let
                                functionEntry =
                                    parseFunctionParameters expression
                            in
                            ( functionEntry
                            , emitFunction functionEntry
                            )
                    in
                    Ok
                        ( emitFunctionStack []
                        , EmitDeclarationBlockResult newEnvFunctionsValues parseAndEmitFunction envFunctionsExpression
                        )


referencedImportsFromRoots :
    List Expression
    -> EmitStack
    -> Dict.Dict String (List String)
    -> List ( String, Expression )
    -> List ( ( List String, String ), EnvironmentFunctionEntry, Pine.Expression )
referencedImportsFromRoots rootDependencies emitStack dependenciesRelations blockDecls =
    let
        prevCompiledFunctions : List ( String, ( EnvironmentFunctionEntry, Pine.Value ) )
        prevCompiledFunctions =
            case Common.assocListGet [] emitStack.importedFunctions of
                Nothing ->
                    []

                Just list ->
                    list

        rootDependenciesNames : List String
        rootDependenciesNames =
            Common.listUnique
                (List.concatMap
                    (\depExpr -> listUnboundReferencesInExpression depExpr [])
                    rootDependencies
                )

        allLocalDependencies : List String
        allLocalDependencies =
            getTransitiveDependencies
                dependenciesRelations
                rootDependenciesNames

        rootDependenciesImportedNames : List ( List String, String )
        rootDependenciesImportedNames =
            List.concatMap
                (\rootDep -> listImportingReferencesInExpression rootDep)
                rootDependencies

        blockDeclsImportedNames : List ( List String, String )
        blockDeclsImportedNames =
            List.concatMap
                (\( _, declExpr ) -> listImportingReferencesInExpression declExpr)
                blockDecls

        allImportedNames : List ( List String, String )
        allImportedNames =
            Common.listUnique
                (List.concat
                    [ rootDependenciesImportedNames
                    , blockDeclsImportedNames
                    , List.concatMap
                        (\depName ->
                            case Common.assocListGet depName blockDecls of
                                Nothing ->
                                    case Common.assocListGet ( [], depName ) emitStack.environmentFunctions of
                                        Nothing ->
                                            case Common.assocListGet depName prevCompiledFunctions of
                                                Nothing ->
                                                    [ ( [], depName ) ]

                                                Just ( EnvironmentFunctionEntry _ prevCompiledEnv, _ ) ->
                                                    case prevCompiledEnv of
                                                        LocalEnvironment localDeps ->
                                                            ( [], depName ) :: localDeps

                                                        _ ->
                                                            [ ( [], depName ) ]

                                        Just (EnvironmentFunctionEntry _ expectedEnv) ->
                                            case expectedEnv of
                                                LocalEnvironment localEnvDeps ->
                                                    ( [], depName ) :: localEnvDeps

                                                ImportedEnvironment _ ->
                                                    [ ( [], depName ) ]

                                                IndependentEnvironment ->
                                                    [ ( [], depName ) ]

                                Just blockDecl ->
                                    listImportingReferencesInExpression blockDecl
                        )
                        allLocalDependencies
                    ]
                )

        referencedImports : List ( ( List String, String ), EnvironmentFunctionEntry, Pine.Expression )
        referencedImports =
            List.foldl
                (\( moduleName, declName ) aggregate ->
                    case Common.assocListGet moduleName emitStack.importedFunctions of
                        Nothing ->
                            aggregate

                        Just moduleDecls ->
                            case Common.assocListGet declName moduleDecls of
                                Nothing ->
                                    aggregate

                                Just ( availableEmitted, emittedValue ) ->
                                    ( ( moduleName, declName )
                                    , availableEmitted
                                    , Pine.LiteralExpression emittedValue
                                    )
                                        :: aggregate
                )
                []
                allImportedNames
    in
    referencedImports


{-| Searches the tree of subexpressions for any that might require adaptive application.
-}
expressionNeedsAdaptiveApplication : List ( ( List String, String ), Int ) -> Expression -> Bool
expressionNeedsAdaptiveApplication knownFunctionParamCounts expression =
    {-
       This function seems brittle because it needs to match the behavior of others, such as emitFunctionApplication.
       Changing something in the selection for inlining might require changes here as well.
       Perhaps it is better to somehow reuse the same logic here.
    -}
    case expression of
        LiteralExpression _ ->
            False

        ListExpression list ->
            List.any
                (\item -> expressionNeedsAdaptiveApplication knownFunctionParamCounts item)
                list

        KernelApplicationExpression _ input ->
            expressionNeedsAdaptiveApplication knownFunctionParamCounts input

        ConditionalExpression condition falseBranch trueBranch ->
            expressionNeedsAdaptiveApplication knownFunctionParamCounts condition
                || expressionNeedsAdaptiveApplication knownFunctionParamCounts trueBranch
                || expressionNeedsAdaptiveApplication knownFunctionParamCounts falseBranch

        FunctionExpression _ functionBody ->
            expressionNeedsAdaptiveApplication knownFunctionParamCounts functionBody

        FunctionApplicationExpression funcExpr args ->
            let
                continueForOtherFunction () =
                    case args of
                        [] ->
                            expressionNeedsAdaptiveApplication knownFunctionParamCounts funcExpr

                        _ ->
                            True
            in
            case funcExpr of
                LiteralExpression _ ->
                    {-
                       Whether that function should be inlined or not, in any case we should not need
                       to supply the generic function for adaptive application.
                    -}
                    List.any
                        (\arg -> expressionNeedsAdaptiveApplication knownFunctionParamCounts arg)
                        args

                ReferenceExpression moduleName declName ->
                    case Common.assocListGet ( moduleName, declName ) knownFunctionParamCounts of
                        Nothing ->
                            continueForOtherFunction ()

                        Just paramCount ->
                            if paramCount == List.length args then
                                expressionNeedsAdaptiveApplication knownFunctionParamCounts funcExpr

                            else
                                continueForOtherFunction ()

                StringTagExpression _ tagged ->
                    expressionNeedsAdaptiveApplication knownFunctionParamCounts tagged

                _ ->
                    continueForOtherFunction ()

        DeclarationBlockExpression declarations innerExpression ->
            let
                blockFunctionParamCounts : List ( ( List String, String ), Int )
                blockFunctionParamCounts =
                    List.map
                        (\( declName, declExpr ) ->
                            ( ( [], declName )
                            , case parseFunctionParameters declExpr of
                                DeclBlockFunctionEntry params _ ->
                                    List.length params
                            )
                        )
                        declarations

                newKnownFunctionParamCounts : List ( ( List String, String ), Int )
                newKnownFunctionParamCounts =
                    List.concat
                        [ blockFunctionParamCounts
                        , knownFunctionParamCounts
                        ]
            in
            List.any
                (\( _, decl ) -> expressionNeedsAdaptiveApplication newKnownFunctionParamCounts decl)
                declarations
                || expressionNeedsAdaptiveApplication newKnownFunctionParamCounts innerExpression

        ReferenceExpression _ _ ->
            False

        StringTagExpression _ tagged ->
            expressionNeedsAdaptiveApplication knownFunctionParamCounts tagged

        PineFunctionApplicationExpression _ argument ->
            expressionNeedsAdaptiveApplication knownFunctionParamCounts argument


{-| Derive an ordered list of recursion domains from a dictionary of dependencies with their transitive dependencies.
Each recursion domain is a set of names of declarations that mutually depend on each other.
The overall list of recursion domains is ordered by their dependencies on each other.
The first element in the list is a set of declarations that do not depend on any other declaration.
-}
recursionDomainsFromDeclarationDependencies : List ( String, Set.Set String ) -> List (Set.Set String)
recursionDomainsFromDeclarationDependencies declarationDependencies =
    let
        depsGroups : List (Set.Set String)
        depsGroups =
            List.map
                (\( declName, declDependencies ) -> Set.insert declName declDependencies)
                declarationDependencies

        depsGroupsUniqueOrdered : List (Set.Set String)
        depsGroupsUniqueOrdered =
            List.sortBy Set.size (Common.listUnique depsGroups)

        deriveDomainsRecursive :
            Set.Set String
            -> List (Set.Set String)
            -> List (Set.Set String)
            -> List (Set.Set String)
        deriveDomainsRecursive covered groups domains =
            case groups of
                [] ->
                    List.reverse domains

                nextGroup :: remainingGroups ->
                    let
                        domain : Set.Set String
                        domain =
                            Set.filter
                                (\name -> not (Set.member name covered))
                                nextGroup

                        coveredNext : Set.Set String
                        coveredNext =
                            Set.union nextGroup covered
                    in
                    deriveDomainsRecursive coveredNext remainingGroups (domain :: domains)
    in
    deriveDomainsRecursive Set.empty depsGroupsUniqueOrdered []


parseFunctionParameters : Expression -> DeclBlockFunctionEntry
parseFunctionParameters expression =
    case expression of
        FunctionExpression functionParams functionBody ->
            let
                (DeclBlockFunctionEntry innerParsedParams innerParsedBody) =
                    parseFunctionParameters functionBody
            in
            DeclBlockFunctionEntry
                (functionParams ++ innerParsedParams)
                innerParsedBody

        StringTagExpression _ tagged ->
            parseFunctionParameters tagged

        _ ->
            DeclBlockFunctionEntry [] expression


emitReferenceExpression : ( List String, String ) -> EmitStack -> Result String Pine.Expression
emitReferenceExpression ( moduleName, declName ) compilation =
    {-
       Prioritize environmentDeconstructions before environmentFunctions here to
       support shadowing for function parameters.
       A source language like Elm does not support shadowing anyway, but the current
       implementation of the Elm compiler sometimes lowers to Fir code that introduces declarations,
       which can result in shadowing when nested.
       An example is the `pseudoParamName` in `compileElmSyntaxCaseBlock`
    -}
    let
        continueWithoutImport () =
            case
                emitApplyFunctionFromCurrentEnvironment compilation ( moduleName, declName ) []
            of
                Nothing ->
                    Err
                        (String.concat
                            [ "Failed referencing '"
                            , String.join "." (List.concat [ moduleName, [ declName ] ])
                            , "'. "
                            , String.fromInt (List.length compilation.environmentDeconstructions)
                            , " deconstructions in scope: "
                            , String.join ", " (List.map Tuple.first compilation.environmentDeconstructions)
                            , ". "
                            , String.fromInt (List.length compilation.environmentFunctions)
                            , " functions in scope: "
                            , String.join
                                ", "
                                (List.map
                                    (\( ( availableModuleName, localName ), _ ) ->
                                        String.join "." (List.concat [ availableModuleName, [ localName ] ])
                                    )
                                    compilation.environmentFunctions
                                )
                            ]
                        )

                Just (Err err) ->
                    Err ("Failed emitting reference as function application: " ++ err)

                Just (Ok functionApplicationOk) ->
                    Ok functionApplicationOk

        continueWithoutDecons () =
            case Common.assocListGet moduleName compilation.importedFunctionsToInline of
                Nothing ->
                    continueWithoutImport ()

                Just importedModule ->
                    case Common.assocListGet declName importedModule of
                        Nothing ->
                            continueWithoutImport ()

                        Just importedFunction ->
                            Ok (Pine.LiteralExpression importedFunction)
    in
    case moduleName of
        [] ->
            case Common.assocListGet declName compilation.environmentDeconstructions of
                Just deconstruction ->
                    Ok
                        (pineExpressionForDeconstructions deconstruction
                            (listItemFromIndexExpression_Pine 1 Pine.environmentExpr)
                        )

                Nothing ->
                    continueWithoutDecons ()

        _ ->
            continueWithoutDecons ()


listTransitiveDependenciesOfExpression : Dict.Dict String (List String) -> Expression -> List String
listTransitiveDependenciesOfExpression dependenciesRelations expression =
    getTransitiveDependencies dependenciesRelations
        (listUnboundReferencesInExpression expression [])


listUnboundReferencesInExpression : Expression -> List String -> List String
listUnboundReferencesInExpression expression boundNames =
    case expression of
        LiteralExpression _ ->
            []

        ListExpression list ->
            List.concatMap
                (\item -> listUnboundReferencesInExpression item boundNames)
                list

        KernelApplicationExpression _ input ->
            listUnboundReferencesInExpression input boundNames

        ConditionalExpression condition falseBranch trueBranch ->
            List.concat
                [ listUnboundReferencesInExpression condition boundNames
                , listUnboundReferencesInExpression falseBranch boundNames
                , listUnboundReferencesInExpression trueBranch boundNames
                ]

        ReferenceExpression moduleName reference ->
            case moduleName of
                [] ->
                    if List.member reference boundNames then
                        []

                    else
                        [ reference ]

                _ ->
                    []

        FunctionExpression functionParam functionBody ->
            let
                functionParamNames : List String
                functionParamNames =
                    List.concatMap
                        (\param -> List.map Tuple.first param)
                        functionParam
            in
            listUnboundReferencesInExpression
                functionBody
                (List.concat [ boundNames, functionParamNames ])

        FunctionApplicationExpression functionExpression arguments ->
            List.concat
                [ listUnboundReferencesInExpression functionExpression boundNames
                , List.concatMap
                    (\argument -> listUnboundReferencesInExpression argument boundNames)
                    arguments
                ]

        DeclarationBlockExpression declarations innerExpression ->
            let
                declarationsNames : List String
                declarationsNames =
                    List.map Tuple.first declarations

                newBoundNames =
                    List.concat [ boundNames, declarationsNames ]
            in
            List.concat
                [ listUnboundReferencesInExpression innerExpression newBoundNames
                , List.concatMap
                    (\( _, decl ) -> listUnboundReferencesInExpression decl newBoundNames)
                    declarations
                ]

        StringTagExpression _ tagged ->
            listUnboundReferencesInExpression tagged boundNames

        PineFunctionApplicationExpression _ argument ->
            listUnboundReferencesInExpression argument boundNames


listImportingReferencesInExpression : Expression -> List ( List String, String )
listImportingReferencesInExpression expression =
    case expression of
        LiteralExpression _ ->
            []

        ListExpression list ->
            List.concatMap
                (\item -> listImportingReferencesInExpression item)
                list

        KernelApplicationExpression _ input ->
            listImportingReferencesInExpression input

        ConditionalExpression condition falseBranch trueBranch ->
            List.concat
                [ listImportingReferencesInExpression condition
                , listImportingReferencesInExpression falseBranch
                , listImportingReferencesInExpression trueBranch
                ]

        ReferenceExpression moduleName reference ->
            case moduleName of
                [] ->
                    []

                _ ->
                    [ ( moduleName, reference ) ]

        FunctionExpression _ functionBody ->
            listImportingReferencesInExpression
                functionBody

        FunctionApplicationExpression functionExpression arguments ->
            List.concat
                [ listImportingReferencesInExpression functionExpression
                , List.concatMap
                    (\argument -> listImportingReferencesInExpression argument)
                    arguments
                ]

        DeclarationBlockExpression declarations innerExpression ->
            List.concat
                [ listImportingReferencesInExpression innerExpression
                , List.concatMap
                    (\( _, decl ) -> listImportingReferencesInExpression decl)
                    declarations
                ]

        StringTagExpression _ tagged ->
            listImportingReferencesInExpression tagged

        PineFunctionApplicationExpression _ argument ->
            listImportingReferencesInExpression argument


getTransitiveDependencies : Dict.Dict String (List String) -> List String -> List String
getTransitiveDependencies dependencies roots =
    mergeDependenciesRecursive dependencies [] roots


mergeDependenciesRecursive :
    Dict.Dict String (List String)
    -> List String
    -> List String
    -> List String
mergeDependenciesRecursive dependencies covered current =
    let
        beforeFilter =
            List.concatMap
                (\reference ->
                    case Dict.get reference dependencies of
                        Nothing ->
                            []

                        Just declDeps ->
                            declDeps
                )
                current

        nextCovered =
            List.concat [ covered, current ]

        newRefs =
            List.filter
                (\dep -> not (List.member dep nextCovered))
                beforeFilter
    in
    case newRefs of
        [] ->
            List.concat [ newRefs, nextCovered ]

        _ ->
            mergeDependenciesRecursive
                dependencies
                nextCovered
                newRefs


pineExpressionForDeconstructions : List Deconstruction -> Pine.Expression -> Pine.Expression
pineExpressionForDeconstructions deconstructions expression =
    List.foldl
        pineExpressionForDeconstruction
        expression
        deconstructions


pineExpressionForDeconstruction : Deconstruction -> Pine.Expression -> Pine.Expression
pineExpressionForDeconstruction deconstruction =
    case deconstruction of
        ListItemDeconstruction index ->
            listItemFromIndexExpression_Pine index

        SkipItemsDeconstruction count ->
            listSkipExpression_Pine count

        PineFunctionApplicationDeconstruction pineFunctionExpression ->
            \emittedArgument ->
                attemptReduceParseAndEvalExpressionRecursive
                    3
                    ( pineFunctionExpression
                        |> Pine.encodeExpressionAsValue
                        |> Pine.LiteralExpression
                    , emittedArgument
                    )


closureParameterFromParameters : Int -> List FunctionParam -> FunctionParam
closureParameterFromParameters offset parameters =
    List.concat
        (List.indexedMap
            (\paramIndex paramDeconstructions ->
                List.map
                    (\( paramName, paramDecons ) ->
                        ( paramName
                        , ListItemDeconstruction (offset + paramIndex) :: paramDecons
                        )
                    )
                    paramDeconstructions
            )
            parameters
        )


consolidateNestedFunctionApplications : Expression -> List Expression -> ( Expression, List Expression )
consolidateNestedFunctionApplications functionExpression arguments =
    case functionExpression of
        FunctionApplicationExpression innerFunction innerArguments ->
            consolidateNestedFunctionApplications
                innerFunction
                (List.concat [ innerArguments, arguments ])

        _ ->
            ( functionExpression, arguments )


emitFunctionApplication : Expression -> List Expression -> EmitStack -> Result String Pine.Expression
emitFunctionApplication functionExpression arguments compilation =
    case arguments of
        [] ->
            emitExpression compilation functionExpression

        _ ->
            case
                Common.resultListIndexedMapCombine
                    (\( argumentIndex, argumentExpression ) ->
                        case emitExpression compilation argumentExpression of
                            Err err ->
                                Err
                                    ("Failed emitting argument "
                                        ++ String.fromInt argumentIndex
                                        ++ " for function application: "
                                        ++ err
                                    )

                            Ok result ->
                                Ok result
                    )
                    arguments
            of
                Err err ->
                    Err err

                Ok argumentsPine ->
                    let
                        genericFunctionApplication () =
                            case emitExpression compilation functionExpression of
                                Err err ->
                                    Err ("Failed emitting function expression: " ++ err)

                                Ok functionExpressionPine ->
                                    emitFunctionApplicationPine compilation argumentsPine functionExpressionPine
                    in
                    case functionExpression of
                        FunctionExpression params funcBody ->
                            if List.length params /= List.length argumentsPine then
                                genericFunctionApplication ()

                            else
                                let
                                    funcBodyRefs : List String
                                    funcBodyRefs =
                                        Common.listUnique
                                            (listUnboundReferencesInExpression funcBody [])

                                    closureCaptures : List ( String, EnvironmentDeconstructionEntry )
                                    closureCaptures =
                                        List.filter
                                            (\( declName, _ ) ->
                                                List.member declName funcBodyRefs
                                            )
                                            compilation.environmentDeconstructions

                                    closureItemsExpressions : List Pine.Expression
                                    closureItemsExpressions =
                                        List.map
                                            (\( _, deconstruction ) ->
                                                pineExpressionForDeconstructions
                                                    deconstruction
                                                    (listItemFromIndexExpression_Pine 1 Pine.environmentExpr)
                                            )
                                            closureCaptures

                                    paramsEnvDeconstructions : FunctionParam
                                    paramsEnvDeconstructions =
                                        closureParameterFromParameters (List.length closureCaptures) params

                                    closureCapturesEnvDeconstructions : FunctionParam
                                    closureCapturesEnvDeconstructions =
                                        List.indexedMap
                                            (\ci ( declName, _ ) ->
                                                ( declName
                                                , [ ListItemDeconstruction ci ]
                                                )
                                            )
                                            closureCaptures

                                    environmentDeconstructions : FunctionParam
                                    environmentDeconstructions =
                                        List.concat
                                            [ closureCapturesEnvDeconstructions
                                            , paramsEnvDeconstructions
                                            ]

                                    prevEnvFunctionsExpr : Pine.Expression
                                    prevEnvFunctionsExpr =
                                        listItemFromIndexExpression_Pine 0 Pine.environmentExpr

                                    argumentsPineWithClosure : List Pine.Expression
                                    argumentsPineWithClosure =
                                        List.concat
                                            [ closureItemsExpressions
                                            , argumentsPine
                                            ]

                                    newEmitStack : EmitStack
                                    newEmitStack =
                                        { compilation
                                            | environmentDeconstructions = environmentDeconstructions
                                            , environmentFunctions = compilation.environmentFunctions
                                        }
                                in
                                case emitExpression newEmitStack funcBody of
                                    Err err ->
                                        Err ("Failed emitting function body: " ++ err)

                                    Ok funcBodyEmitted ->
                                        Ok
                                            (Pine.ParseAndEvalExpression
                                                (Pine.LiteralExpression
                                                    (Pine.encodeExpressionAsValue funcBodyEmitted)
                                                )
                                                (Pine.ListExpression
                                                    [ prevEnvFunctionsExpr
                                                    , Pine.ListExpression argumentsPineWithClosure
                                                    ]
                                                )
                                            )

                        ReferenceExpression moduleName functionName ->
                            case
                                emitApplyFunctionFromCurrentEnvironment
                                    compilation
                                    ( moduleName, functionName )
                                    argumentsPine
                            of
                                Nothing ->
                                    genericFunctionApplication ()

                                Just (Err err) ->
                                    Err
                                        ("Failed emitting function application with "
                                            ++ String.fromInt (List.length arguments)
                                            ++ " arguments: "
                                            ++ err
                                        )

                                Just (Ok ok) ->
                                    Ok ok

                        _ ->
                            genericFunctionApplication ()


emitFunctionApplicationPine :
    EmitStack
    -> List Pine.Expression
    -> Pine.Expression
    -> Result String Pine.Expression
emitFunctionApplicationPine emitStack arguments functionExpressionPine =
    let
        genericPartialApplication () =
            partialApplicationExpressionFromListOfArguments
                arguments
                emitStack
                functionExpressionPine
    in
    if emitStack.skipApplyFunction then
        Ok (genericPartialApplication ())

    else if not (pineExpressionIsIndependent functionExpressionPine) then
        Ok (genericPartialApplication ())

    else
        case evaluateAsIndependentExpression functionExpressionPine of
            Err err ->
                Err err

            Ok functionRecordValue ->
                case parseFunctionRecordFromValueTagged functionRecordValue of
                    Err _ ->
                        Ok (genericPartialApplication ())

                    Ok (ParsedFunctionValue _ parseInnerFunction parameterCount (ParsedFunctionEnvFunctions envFunctions) argumentsAlreadyCollected) ->
                        let
                            combinedArguments =
                                List.concat
                                    [ List.map Pine.LiteralExpression argumentsAlreadyCollected
                                    , arguments
                                    ]
                        in
                        if parameterCount /= List.length combinedArguments then
                            Ok (genericPartialApplication ())

                        else
                            case parseInnerFunction () of
                                Err err ->
                                    Err ("Failed to parse inner function: " ++ err)

                                Ok innerFunction ->
                                    let
                                        mappedEnvironment =
                                            Pine.ListExpression
                                                [ Pine.ListExpression
                                                    (List.map Pine.LiteralExpression envFunctions)
                                                , Pine.ListExpression combinedArguments
                                                ]

                                        findReplacementForExpression expression =
                                            case expression of
                                                Pine.EnvironmentExpression ->
                                                    Just mappedEnvironment

                                                _ ->
                                                    Nothing

                                        ( afterReplace, _ ) =
                                            transformPineExpressionWithOptionalReplacement
                                                findReplacementForExpression
                                                innerFunction
                                    in
                                    Ok
                                        (searchForExpressionReductionRecursive
                                            5
                                            afterReplace
                                        )


emitApplyFunctionFromCurrentEnvironment :
    EmitStack
    -> ( List String, String )
    -> List Pine.Expression
    -> Maybe (Result String Pine.Expression)
emitApplyFunctionFromCurrentEnvironment compilation ( moduleName, functionName ) arguments =
    let
        currentEnvironmentFunctionEntryFromName : ( List String, String ) -> Maybe ( Int, EnvironmentFunctionEntry )
        currentEnvironmentFunctionEntryFromName name =
            Common.assocListGetWithIndex name compilation.environmentFunctions
    in
    case currentEnvironmentFunctionEntryFromName ( moduleName, functionName ) of
        Nothing ->
            Nothing

        Just ( functionIndexInEnv, EnvironmentFunctionEntry funcParamCount funcExpectedEnv ) ->
            let
                getEnvFunctionsExpression =
                    listItemFromIndexExpression_Pine
                        0
                        Pine.environmentExpr

                getFunctionExpression =
                    listItemFromIndexExpression_Pine
                        functionIndexInEnv
                        getEnvFunctionsExpression
            in
            case funcExpectedEnv of
                ImportedEnvironment pathToRecordFromEnvEntry ->
                    let
                        funcRecordLessTag =
                            pineExpressionForDeconstructions
                                pathToRecordFromEnvEntry
                                getFunctionExpression

                        {-
                           The paths here mirror the composition in 'buildRecordOfPartiallyAppliedFunction'
                        -}
                        importedGetFunctionExpr =
                            pineExpressionForDeconstructions
                                [ ListItemDeconstruction 1
                                , ListItemDeconstruction 0
                                ]
                                funcRecordLessTag

                        importedGetEnvFunctionsExpression =
                            pineExpressionForDeconstructions
                                [ ListItemDeconstruction 1
                                , ListItemDeconstruction 2
                                ]
                                funcRecordLessTag
                    in
                    Just
                        (Ok
                            (if funcParamCount == List.length arguments then
                                Pine.ParseAndEvalExpression
                                    importedGetFunctionExpr
                                    (Pine.ListExpression
                                        [ importedGetEnvFunctionsExpression
                                        , Pine.ListExpression arguments
                                        ]
                                    )

                             else
                                partialApplicationExpressionFromListOfArguments
                                    arguments
                                    compilation
                                    funcRecordLessTag
                            )
                        )

                LocalEnvironment localEnvExpectedDecls ->
                    let
                        buildEnvironmentRecursive :
                            List Pine.Expression
                            -> List ( List String, String )
                            -> Result String Pine.Expression
                        buildEnvironmentRecursive alreadyMapped remaining =
                            case remaining of
                                [] ->
                                    Ok (Pine.ListExpression alreadyMapped)

                                qualifiedName :: remainingNext ->
                                    case currentEnvironmentFunctionEntryFromName qualifiedName of
                                        Nothing ->
                                            let
                                                ( expectModuleName, expectDeclName ) =
                                                    qualifiedName
                                            in
                                            Err
                                                ("Function '"
                                                    ++ functionName
                                                    ++ "' expects environment function '"
                                                    ++ String.join "."
                                                        (List.concat [ expectModuleName, [ expectDeclName ] ])
                                                    ++ "' but it is not in the environment"
                                                )

                                        Just ( indexInEnv, _ ) ->
                                            buildEnvironmentRecursive
                                                (List.concat
                                                    [ alreadyMapped
                                                    , [ listItemFromIndexExpression_Pine
                                                            indexInEnv
                                                            getEnvFunctionsExpression
                                                      ]
                                                    ]
                                                )
                                                remainingNext

                        buildExpectedEnvironmentResult =
                            if localEnvExpectedDecls == [] then
                                Ok (Pine.ListExpression [])

                            else
                                let
                                    currentEnv : List ( List String, String )
                                    currentEnv =
                                        List.map Tuple.first compilation.environmentFunctions

                                    currentEnvCoversExpected : Bool
                                    currentEnvCoversExpected =
                                        List.take (List.length localEnvExpectedDecls) currentEnv == localEnvExpectedDecls
                                in
                                if currentEnvCoversExpected then
                                    Ok getEnvFunctionsExpression

                                else
                                    buildEnvironmentRecursive [] localEnvExpectedDecls
                    in
                    case buildExpectedEnvironmentResult of
                        Err err ->
                            Just (Err err)

                        Ok expectedEnvironment ->
                            Just
                                (Ok
                                    (if funcParamCount == List.length arguments then
                                        Pine.ParseAndEvalExpression
                                            getFunctionExpression
                                            (Pine.ListExpression
                                                [ expectedEnvironment
                                                , Pine.ListExpression arguments
                                                ]
                                            )

                                     else
                                        partialApplicationExpressionFromListOfArguments
                                            arguments
                                            compilation
                                            (case funcParamCount of
                                                0 ->
                                                    Pine.ParseAndEvalExpression
                                                        getFunctionExpression
                                                        (Pine.ListExpression
                                                            [ expectedEnvironment
                                                            , Pine.ListExpression []
                                                            ]
                                                        )

                                                _ ->
                                                    buildRecordOfPartiallyAppliedFunction
                                                        { getFunctionInnerExpression = getFunctionExpression
                                                        , getEnvFunctionsExpression = expectedEnvironment
                                                        , parameterCount = funcParamCount
                                                        , argumentsAlreadyCollected = []
                                                        }
                                            )
                                    )
                                )

                IndependentEnvironment ->
                    Just
                        (Ok
                            (partialApplicationExpressionFromListOfArguments
                                arguments
                                compilation
                                getFunctionExpression
                            )
                        )


partialApplicationExpressionFromListOfArguments :
    List Pine.Expression
    -> EmitStack
    -> Pine.Expression
    -> Pine.Expression
partialApplicationExpressionFromListOfArguments arguments emitStack function =
    case arguments of
        [] ->
            function

        _ ->
            adaptivePartialApplicationExpression
                { function = function
                , arguments = arguments
                , applicationFunctionSource =
                    case emitReferenceExpression ( [], environmentFunctionPartialApplicationName ) emitStack of
                        Err _ ->
                            Nothing

                        Ok appFunc ->
                            Just appFunc
                }


emitWrapperForPartialApplication : Pine.Expression -> Int -> Pine.Expression -> Pine.Expression
emitWrapperForPartialApplication envFunctionsExpression parameterCount innerExpression =
    case parameterCount of
        0 ->
            emitWrapperForPartialApplicationZero
                { getFunctionInnerExpression =
                    innerExpression
                        |> Pine.encodeExpressionAsValue
                        |> Pine.LiteralExpression
                , getEnvFunctionsExpression = envFunctionsExpression
                }

        _ ->
            buildRecordOfPartiallyAppliedFunction
                { getFunctionInnerExpression =
                    innerExpression
                        |> Pine.encodeExpressionAsValue
                        |> Pine.LiteralExpression
                , parameterCount = parameterCount
                , getEnvFunctionsExpression = envFunctionsExpression
                , argumentsAlreadyCollected = []
                }


emitWrapperForPartialApplicationZero :
    { getFunctionInnerExpression : Pine.Expression
    , getEnvFunctionsExpression : Pine.Expression
    }
    -> Pine.Expression
emitWrapperForPartialApplicationZero { getFunctionInnerExpression, getEnvFunctionsExpression } =
    Pine.ParseAndEvalExpression
        getFunctionInnerExpression
        (Pine.ListExpression
            [ getEnvFunctionsExpression
            , Pine.ListExpression []
            ]
        )


adaptivePartialApplicationExpression :
    { function : Pine.Expression
    , arguments : List Pine.Expression
    , applicationFunctionSource : Maybe Pine.Expression
    }
    -> Pine.Expression
adaptivePartialApplicationExpression config =
    case config.arguments of
        [] ->
            config.function

        _ ->
            let
                applicationFunctionExpr =
                    case config.applicationFunctionSource of
                        Just applicationFunctionSource ->
                            applicationFunctionSource

                        Nothing ->
                            Pine.LiteralExpression (adaptivePartialApplicationRecursiveValue ())
            in
            Pine.ParseAndEvalExpression
                applicationFunctionExpr
                (Pine.ListExpression
                    [ applicationFunctionExpr
                    , config.function
                    , Pine.ListExpression config.arguments
                    ]
                )


adaptivePartialApplicationRecursiveValue : () -> Pine.Value
adaptivePartialApplicationRecursiveValue () =
    Pine.encodeExpressionAsValue adaptivePartialApplicationRecursiveExpression


{-| In adaptive (partial) function application, we check whether the function is a structured function record or not.
-}
adaptivePartialApplicationRecursiveExpression : Pine.Expression
adaptivePartialApplicationRecursiveExpression =
    let
        selfFunctionLocalExpression =
            listItemFromIndexExpression_Pine 0 Pine.environmentExpr

        functionLocalExpression =
            listItemFromIndexExpression_Pine 1 Pine.environmentExpr

        remainingArgumentsLocalExpression =
            listItemFromIndexExpression_Pine 2 Pine.environmentExpr

        nextArgumentLocalExpression =
            listItemFromIndexExpression_Pine 0 remainingArgumentsLocalExpression

        applyNextExpression =
            Pine.ConditionalExpression
                {-
                   If the first element in 'function' equals 'Function',
                -}
                (equalCondition_Pine
                    [ listItemFromIndexExpression_Pine 0 functionLocalExpression
                    , Pine.LiteralExpression Pine.stringAsValue_Function
                    ]
                )
                (attemptReduceParseAndEvalExpressionRecursiveWithDefaultDepth
                    ( functionLocalExpression
                    , nextArgumentLocalExpression
                    )
                )
                {-
                   assume the second list item is a list with the following items:
                   + 0: inner function
                   + 1: number of parameters expected by the inner function
                   + 2: captured environment functions
                   + 3: the arguments collected so far.
                -}
                (let
                    partiallyAppliedFunctionRecord =
                        listItemFromIndexExpression_Pine 1 functionLocalExpression

                    innerFunction =
                        listItemFromIndexExpression_Pine 0
                            partiallyAppliedFunctionRecord

                    numberOfParametersExpectedByInnerFunction =
                        listItemFromIndexExpression_Pine 1
                            partiallyAppliedFunctionRecord

                    environmentFunctions =
                        listItemFromIndexExpression_Pine 2
                            partiallyAppliedFunctionRecord

                    previouslyCollectedArguments =
                        listItemFromIndexExpression_Pine 3
                            partiallyAppliedFunctionRecord

                    collectedArguments =
                        Pine.KernelApplicationExpression
                            "concat"
                            (Pine.ListExpression
                                [ previouslyCollectedArguments
                                , Pine.ListExpression [ nextArgumentLocalExpression ]
                                ]
                            )

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
                    collectedArgumentsAreComplete
                    -- If it is not, we need to collect more arguments.
                    (updateRecordOfPartiallyAppliedFunction
                        { getFunctionInnerExpression = innerFunction
                        , parameterCountExpression = numberOfParametersExpectedByInnerFunction
                        , getEnvFunctionsExpression = environmentFunctions
                        , argumentsAlreadyCollectedExpression = collectedArguments
                        }
                    )
                    -- If it is, we can apply the inner function.
                    (Pine.ParseAndEvalExpression
                        innerFunction
                        (Pine.ListExpression
                            [ environmentFunctions
                            , collectedArguments
                            ]
                        )
                    )
                )
    in
    Pine.ConditionalExpression
        (equalCondition_Pine
            [ Pine.ListExpression []
            , remainingArgumentsLocalExpression
            ]
        )
        (Pine.ParseAndEvalExpression
            selfFunctionLocalExpression
            (Pine.ListExpression
                [ selfFunctionLocalExpression
                , applyNextExpression
                , listSkipExpression_Pine 1 remainingArgumentsLocalExpression
                ]
            )
        )
        functionLocalExpression


buildRecordOfPartiallyAppliedFunction :
    { getFunctionInnerExpression : Pine.Expression
    , getEnvFunctionsExpression : Pine.Expression
    , parameterCount : Int
    , argumentsAlreadyCollected : List Pine.Expression
    }
    -> Pine.Expression
buildRecordOfPartiallyAppliedFunction config =
    updateRecordOfPartiallyAppliedFunction
        { getFunctionInnerExpression = config.getFunctionInnerExpression
        , getEnvFunctionsExpression = config.getEnvFunctionsExpression
        , parameterCountExpression =
            Pine.LiteralExpression (Pine.valueFromInt config.parameterCount)
        , argumentsAlreadyCollectedExpression = Pine.ListExpression config.argumentsAlreadyCollected
        }


updateRecordOfPartiallyAppliedFunction :
    { getFunctionInnerExpression : Pine.Expression
    , getEnvFunctionsExpression : Pine.Expression
    , parameterCountExpression : Pine.Expression
    , argumentsAlreadyCollectedExpression : Pine.Expression
    }
    -> Pine.Expression
updateRecordOfPartiallyAppliedFunction config =
    Pine.ListExpression
        [ Pine.LiteralExpression Pine.stringAsValue_Function
        , Pine.ListExpression
            [ config.getFunctionInnerExpression
            , config.parameterCountExpression
            , config.getEnvFunctionsExpression
            , config.argumentsAlreadyCollectedExpression
            ]
        ]


parseFunctionRecordFromValueTagged :
    Pine.Value
    -> Result String ParsedFunctionValue
parseFunctionRecordFromValueTagged value =
    case value of
        Pine.BlobValue _ ->
            Err "Is not a list but a blob"

        Pine.ListValue listItems ->
            case listItems of
                [ functionTag, functionRecord ] ->
                    if
                        (functionTag == Pine.stringAsValue_Function_2025)
                            || (functionTag == Pine.stringAsValue_Function_2024)
                    then
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
    -> Result String ParsedFunctionValue
parseFunctionRecordFromValue value =
    case value of
        Pine.ListValue listItems ->
            case listItems of
                [ innerFunctionValue, parameterCountValue, envFunctionsValue, argumentsAlreadyCollectedValue ] ->
                    case Pine.intFromValue parameterCountValue of
                        Err err ->
                            Err ("Failed to parse function parameter count: " ++ err)

                        Ok parameterCount ->
                            case envFunctionsValue of
                                Pine.ListValue envFunctions ->
                                    case argumentsAlreadyCollectedValue of
                                        Pine.ListValue argumentsAlreadyCollected ->
                                            Ok
                                                (ParsedFunctionValue
                                                    innerFunctionValue
                                                    (\() -> Pine.parseExpressionFromValue innerFunctionValue)
                                                    parameterCount
                                                    (ParsedFunctionEnvFunctions envFunctions)
                                                    argumentsAlreadyCollected
                                                )

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


attemptReduceParseAndEvalExpressionRecursiveWithDefaultDepth :
    ( Pine.Expression, Pine.Expression )
    -> Pine.Expression
attemptReduceParseAndEvalExpressionRecursiveWithDefaultDepth ( origEncodedExpr, origEnvExpr ) =
    let
        sizeBeforeReduction =
            countPineExpressionSize estimatePineValueSize origEnvExpr
                + countPineExpressionSize estimatePineValueSize origEncodedExpr

        reductionMaxDepth =
            if sizeBeforeReduction < 10 * 1000 then
                2

            else
                1
    in
    attemptReduceParseAndEvalExpressionRecursive
        reductionMaxDepth
        ( origEncodedExpr, origEnvExpr )


attemptReduceParseAndEvalExpressionRecursive :
    Int
    -> ( Pine.Expression, Pine.Expression )
    -> Pine.Expression
attemptReduceParseAndEvalExpressionRecursive maxDepth ( origEncodedExpr, origEnvExpr ) =
    let
        default =
            Pine.ParseAndEvalExpression origEncodedExpr origEnvExpr
    in
    if maxDepth < 1 then
        default

    else
        case searchReductionForParseAndEvalExpression ( origEncodedExpr, origEnvExpr ) of
            Nothing ->
                default

            Just reduced ->
                case reduced of
                    Pine.ParseAndEvalExpression reducedEncodedExpr reducedEnvExpr ->
                        attemptReduceParseAndEvalExpressionRecursive
                            (maxDepth - 1)
                            ( reducedEncodedExpr, reducedEnvExpr )

                    _ ->
                        reduced


searchReductionForParseAndEvalExpression :
    ( Pine.Expression, Pine.Expression )
    -> Maybe Pine.Expression
searchReductionForParseAndEvalExpression ( origEncodedExpr, origEnvExpr ) =
    if pineExpressionIsIndependent origEncodedExpr then
        case Pine.evaluateExpression Pine.emptyEvalEnvironment origEncodedExpr of
            Err _ ->
                Nothing

            Ok expressionValue ->
                case Pine.parseExpressionFromValue expressionValue of
                    Err _ ->
                        Nothing

                    Ok parsedExpression ->
                        let
                            findReplacementForExpression expression =
                                case expression of
                                    Pine.EnvironmentExpression ->
                                        Just origEnvExpr

                                    _ ->
                                        Nothing

                            ( reducedExpr, transformResult ) =
                                transformPineExpressionWithOptionalReplacement
                                    findReplacementForExpression
                                    parsedExpression
                        in
                        if transformResult.referencesOriginalEnvironment then
                            Nothing

                        else
                            Just
                                (searchForExpressionReductionRecursive 5 reducedExpr)

    else
        Nothing


searchForExpressionReductionRecursive : Int -> Pine.Expression -> Pine.Expression
searchForExpressionReductionRecursive maxDepth expression =
    if maxDepth < 1 then
        expression

    else
        let
            ( transformed, _ ) =
                transformPineExpressionWithOptionalReplacement searchForExpressionReduction expression
        in
        if transformed == expression then
            transformed

        else
            searchForExpressionReductionRecursive (maxDepth - 1) transformed


reduceExpressionToLiteralIfIndependent : Pine.Expression -> Pine.Expression
reduceExpressionToLiteralIfIndependent expression =
    if pineExpressionIsIndependent expression then
        case Pine.evaluateExpression Pine.emptyEvalEnvironment expression of
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
                case Pine.evaluateExpression Pine.emptyEvalEnvironment expression of
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

        Pine.KernelApplicationExpression rootFunctionName rootArgument ->
            case rootFunctionName of
                "head" ->
                    case rootArgument of
                        Pine.ListExpression argumentList ->
                            List.head argumentList

                        _ ->
                            attemptReduceViaEval ()

                "skip" ->
                    case rootArgument of
                        Pine.ListExpression [ Pine.LiteralExpression skipCountLiteral, Pine.ListExpression expressionList ] ->
                            case Pine.intFromValue skipCountLiteral of
                                Err _ ->
                                    attemptReduceViaEval ()

                                Ok skipCount ->
                                    Just
                                        (Pine.ListExpression
                                            (List.drop skipCount expressionList)
                                        )

                        _ ->
                            attemptReduceViaEval ()

                _ ->
                    attemptReduceViaEval ()

        Pine.ConditionalExpression condition falseBranch trueBranch ->
            if pineExpressionIsIndependent condition then
                case Pine.evaluateExpression Pine.emptyEvalEnvironment condition of
                    Err _ ->
                        Nothing

                    Ok conditionValue ->
                        if conditionValue == Pine.falseValue then
                            Just falseBranch

                        else if conditionValue == Pine.trueValue then
                            Just trueBranch

                        else
                            Just (Pine.LiteralExpression (Pine.ListValue []))

            else
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
                        ( items, refsOrig ) =
                            List.foldr
                                (\item ( itemsIntermediate, refsOrigIntermediate ) ->
                                    let
                                        ( itemExpr, itemInspect ) =
                                            transformPineExpressionWithOptionalReplacement findReplacement item
                                    in
                                    ( itemExpr :: itemsIntermediate
                                    , refsOrigIntermediate || itemInspect.referencesOriginalEnvironment
                                    )
                                )
                                ( [], False )
                                list
                    in
                    ( Pine.ListExpression items
                    , { referencesOriginalEnvironment = refsOrig
                      }
                    )

                Pine.ParseAndEvalExpression encodedExpr envExpr ->
                    let
                        ( encodedTransform, encodedInspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement encodedExpr

                        ( envTransform, envInspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement envExpr
                    in
                    ( Pine.ParseAndEvalExpression
                        encodedTransform
                        envTransform
                    , { referencesOriginalEnvironment =
                            encodedInspect.referencesOriginalEnvironment
                                || envInspect.referencesOriginalEnvironment
                      }
                    )

                Pine.KernelApplicationExpression functionName argumentOrig ->
                    let
                        ( argument, inspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement argumentOrig
                    in
                    ( Pine.KernelApplicationExpression functionName argument
                    , inspect
                    )

                Pine.ConditionalExpression condition falseBranch trueBranch ->
                    let
                        ( conditionExpr, conditionInspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement condition

                        ( falseBranchExpr, falseBranchInspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement falseBranch

                        ( trueBranchExpr, trueBranchInspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement trueBranch
                    in
                    ( Pine.ConditionalExpression
                        conditionExpr
                        falseBranchExpr
                        trueBranchExpr
                    , { referencesOriginalEnvironment =
                            conditionInspect.referencesOriginalEnvironment
                                || falseBranchInspect.referencesOriginalEnvironment
                                || trueBranchInspect.referencesOriginalEnvironment
                      }
                    )

                Pine.EnvironmentExpression ->
                    ( Pine.environmentExpr
                    , { referencesOriginalEnvironment = True
                      }
                    )

                Pine.StringTagExpression tag tagged ->
                    let
                        ( taggedTransformed, inspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement tagged
                    in
                    ( Pine.StringTagExpression tag taggedTransformed, inspect )


listFunctionAppExpressions : Expression -> List ( Expression, List Expression )
listFunctionAppExpressions expr =
    case expr of
        FunctionApplicationExpression funcExpr args ->
            List.concat
                [ ( funcExpr, args ) :: listFunctionAppExpressions funcExpr
                , List.concatMap listFunctionAppExpressions args
                ]

        LiteralExpression _ ->
            []

        ListExpression list ->
            List.concatMap listFunctionAppExpressions list

        KernelApplicationExpression _ argument ->
            listFunctionAppExpressions argument

        ConditionalExpression condition falseBranch trueBranch ->
            List.concat
                [ listFunctionAppExpressions condition
                , listFunctionAppExpressions falseBranch
                , listFunctionAppExpressions trueBranch
                ]

        FunctionExpression _ functionBody ->
            listFunctionAppExpressions functionBody

        ReferenceExpression _ _ ->
            []

        DeclarationBlockExpression declarations innerExpression ->
            List.concat
                [ List.concatMap listFunctionAppExpressions (List.map Tuple.second declarations)
                , listFunctionAppExpressions innerExpression
                ]

        StringTagExpression _ tagged ->
            listFunctionAppExpressions tagged

        PineFunctionApplicationExpression _ argument ->
            listFunctionAppExpressions argument


inlineLocalReferences : List ( String, Expression ) -> Expression -> Expression
inlineLocalReferences references expression =
    case expression of
        FunctionApplicationExpression funcExpr args ->
            FunctionApplicationExpression
                (inlineLocalReferences references funcExpr)
                (List.map (inlineLocalReferences references) args)

        LiteralExpression _ ->
            expression

        ListExpression list ->
            ListExpression (List.map (inlineLocalReferences references) list)

        KernelApplicationExpression funcName argument ->
            KernelApplicationExpression funcName (inlineLocalReferences references argument)

        ConditionalExpression condition falseBranch trueBranch ->
            ConditionalExpression
                (inlineLocalReferences references condition)
                (inlineLocalReferences references falseBranch)
                (inlineLocalReferences references trueBranch)

        FunctionExpression params functionBody ->
            let
                allParamNames =
                    List.concatMap (List.map Tuple.first) params

                referencesForInnerExpression =
                    {-
                       Implement shadowing by removing the declarations from the references list.
                    -}
                    List.filter
                        (\( name, _ ) ->
                            not (List.member name allParamNames)
                        )
                        references
            in
            FunctionExpression
                params
                (inlineLocalReferences referencesForInnerExpression functionBody)

        ReferenceExpression moduleName functionName ->
            if moduleName == [] then
                case Common.assocListGet functionName references of
                    Nothing ->
                        ReferenceExpression moduleName functionName

                    Just inlinedExpr ->
                        inlineLocalReferences references inlinedExpr

            else
                ReferenceExpression moduleName functionName

        DeclarationBlockExpression declarations innerExpression ->
            let
                referencesForInnerExpression =
                    {-
                       Implement shadowing by removing the declarations from the references list.
                    -}
                    List.filter
                        (\( name, _ ) ->
                            not (List.member name (List.map Tuple.first declarations))
                        )
                        references
            in
            DeclarationBlockExpression
                (List.map
                    (\( name, declExpr ) ->
                        ( name, inlineLocalReferences references declExpr )
                    )
                    declarations
                )
                (inlineLocalReferences referencesForInnerExpression innerExpression)

        StringTagExpression tag tagged ->
            StringTagExpression tag (inlineLocalReferences references tagged)

        PineFunctionApplicationExpression pineExpr argExpr ->
            PineFunctionApplicationExpression pineExpr (inlineLocalReferences references argExpr)


evaluateAsIndependentExpression : Pine.Expression -> Result String Pine.Value
evaluateAsIndependentExpression expression =
    if not (pineExpressionIsIndependent expression) then
        Err "Expression is not independent"

    else
        case Pine.evaluateExpression Pine.emptyEvalEnvironment expression of
            Err err ->
                Err
                    ("Expression seems independent but failed to evaluate: "
                        ++ Pine.displayStringFromPineError err
                    )

            Ok value ->
                Ok value


pineExpressionIsIndependent : Pine.Expression -> Bool
pineExpressionIsIndependent expression =
    case expression of
        Pine.LiteralExpression _ ->
            True

        Pine.ListExpression list ->
            List.all pineExpressionIsIndependent list

        Pine.ParseAndEvalExpression encodedExpr envExpr ->
            if pineExpressionIsIndependent encodedExpr then
                pineExpressionIsIndependent envExpr

            else
                False

        Pine.KernelApplicationExpression _ input ->
            pineExpressionIsIndependent input

        Pine.ConditionalExpression condition falseBranch trueBranch ->
            if pineExpressionIsIndependent condition then
                if pineExpressionIsIndependent falseBranch then
                    pineExpressionIsIndependent trueBranch

                else
                    False

            else
                False

        Pine.EnvironmentExpression ->
            False

        Pine.StringTagExpression _ tagged ->
            pineExpressionIsIndependent tagged


listItemFromIndexExpression : Int -> Expression -> Expression
listItemFromIndexExpression itemIndex listExpression =
    pineKernel_Head (listSkipExpression itemIndex listExpression)


countListElementsExpression : Expression -> Expression
countListElementsExpression sequenceExpression =
    KernelApplicationExpression
        "length"
        sequenceExpression


pineKernel_Head : Expression -> Expression
pineKernel_Head listExpression =
    KernelApplicationExpression
        "head"
        listExpression


listSkipExpression : Int -> Expression -> Expression
listSkipExpression numberToDrop listExpression =
    if numberToDrop < 1 then
        listExpression

    else
        applyKernelFunctionWithTwoArguments
            "skip"
            (LiteralExpression (Pine.valueFromInt numberToDrop))
            listExpression


equalCondition : List Expression -> Expression
equalCondition list =
    KernelApplicationExpression
        "equal"
        (ListExpression list)


applyKernelFunctionWithTwoArguments : String -> Expression -> Expression -> Expression
applyKernelFunctionWithTwoArguments kernelFunctionName argA argB =
    KernelApplicationExpression
        kernelFunctionName
        (ListExpression [ argA, argB ])


countListElementsExpression_Pine : Pine.Expression -> Pine.Expression
countListElementsExpression_Pine sequenceExpression =
    Pine.KernelApplicationExpression
        "length"
        sequenceExpression


listItemFromIndexExpression_Pine : Int -> Pine.Expression -> Pine.Expression
listItemFromIndexExpression_Pine itemIndex listExpression =
    pineKernel_Head_Pine (listSkipExpression_Pine itemIndex listExpression)


listSkipExpression_Pine : Int -> Pine.Expression -> Pine.Expression
listSkipExpression_Pine numberToDrop listExpression =
    if numberToDrop < 1 then
        listExpression

    else
        applyKernelFunctionWithTwoArguments_Pine
            "skip"
            (Pine.LiteralExpression (Pine.valueFromInt numberToDrop))
            listExpression


pineKernel_Head_Pine : Pine.Expression -> Pine.Expression
pineKernel_Head_Pine listExpression =
    Pine.KernelApplicationExpression
        "head"
        listExpression


equalCondition_Pine : List Pine.Expression -> Pine.Expression
equalCondition_Pine list =
    Pine.KernelApplicationExpression
        "equal"
        (Pine.ListExpression list)


applyKernelFunctionWithTwoArguments_Pine : String -> Pine.Expression -> Pine.Expression -> Pine.Expression
applyKernelFunctionWithTwoArguments_Pine kernelFunctionName argA argB =
    Pine.KernelApplicationExpression
        kernelFunctionName
        (Pine.ListExpression [ argA, argB ])


countPineExpressionSize : (Pine.Value -> Int) -> Pine.Expression -> Int
countPineExpressionSize countValueSize expression =
    case expression of
        Pine.LiteralExpression literal ->
            countValueSize literal

        Pine.ListExpression list ->
            List.foldl (\item sum -> sum + countPineExpressionSize countValueSize item)
                1
                list

        Pine.ParseAndEvalExpression encodedExpr envExpr ->
            countPineExpressionSize countValueSize encodedExpr
                + countPineExpressionSize countValueSize envExpr

        Pine.KernelApplicationExpression _ input ->
            2 + countPineExpressionSize countValueSize input

        Pine.ConditionalExpression condition falseBranch trueBranch ->
            countPineExpressionSize countValueSize condition
                + countPineExpressionSize countValueSize falseBranch
                + countPineExpressionSize countValueSize trueBranch

        Pine.EnvironmentExpression ->
            1

        Pine.StringTagExpression _ tagged ->
            countPineExpressionSize countValueSize tagged


estimatePineValueSize : Pine.Value -> Int
estimatePineValueSize value =
    let
        ( nodeCount, byteCount ) =
            Pine.countValueContent value
    in
    10 + nodeCount * 10 + byteCount
