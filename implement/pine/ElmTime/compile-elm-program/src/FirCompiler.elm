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
    , listFunctionAppExpressions
    , listItemFromIndexExpression
    , listItemFromIndexExpression_Pine
    , listSkipExpression
    , listSkipExpression_Pine
    , listTransitiveDependenciesOfExpression
    , listUnboundReferencesInExpression
    , parseFunctionParameters
    , parseFunctionRecordFromValueTagged
    , partialApplicationExpressionFromListOfArguments
    , pineExpressionIsIndependent
    , pineKernel_ListHead
    , pineKernel_ListHead_Pine
    , recursionDomainsFromDeclarationDependencies
    )

import Common
import Dict
import Pine
import Set


type Expression
    = LiteralExpression Pine.Value
    | ListExpression (List Expression)
    | KernelApplicationExpression Expression String
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
    , declarationsDependencies : Dict.Dict String (Set.Set String)

    -- The functions in the first item in the environment list
    , environmentFunctions : List ( ( List String, String ), EnvironmentFunctionEntry )

    -- Deconstructions we can derive from the second item in the environment list
    , environmentDeconstructions : List ( String, EnvironmentDeconstructionEntry )
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

        KernelApplicationExpression argumentFir functionName ->
            case emitExpression stack argumentFir of
                Err err ->
                    Err err

                Ok argument ->
                    Ok (Pine.KernelApplicationExpression argument functionName)

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
            emitFunctionApplication functionExpression arguments stack

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
                            ( emittedArgument
                            , Pine.LiteralExpression
                                (Pine.encodeExpressionAsValue pineFunctionExpression)
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
emitExpressionInDeclarationBlock stackBeforeAddingDeps blockDeclarations mainExpression =
    let
        stackBefore =
            { stackBeforeAddingDeps
                | declarationsDependencies =
                    List.foldl
                        (\( declName, declExpression ) aggregate ->
                            Dict.insert declName (listUnboundReferencesInExpression declExpression []) aggregate
                        )
                        stackBeforeAddingDeps.declarationsDependencies
                        blockDeclarations
            }

        mainExpressionOuterDependencies : List String
        mainExpressionOuterDependencies =
            Set.toList (listTransitiveDependenciesOfExpression stackBefore mainExpression)

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
                (\( declName, deconstruction ) ->
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
                mainExpressionImports : Set.Set ( List String, String )
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
                        (Set.toList mainExpressionImports)
            in
            case mainDependsOnImport of
                False ->
                    let
                        needsAdaptiveApplication =
                            case stackBefore.environmentFunctions of
                                [] ->
                                    expressionNeedsAdaptiveApplication mainExprInnerExpr

                                _ ->
                                    {-
                                       If the stackBefore.environmentFunctions is not empty, assume we already added nessecary internals
                                       in a parent scope.
                                    -}
                                    False
                    in
                    case needsAdaptiveApplication of
                        True ->
                            continueEmitBlock ()

                        False ->
                            emitExpression stackBeforeAddingDeps mainExprInnerExpr

                True ->
                    continueEmitBlock ()

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

        availableEmittedDependencies : Dict.Dict String (Set.Set String)
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
                                                (Set.fromList localEnvExpectedDeclsLocal)
                                                aggregate

                                ImportedEnvironment _ ->
                                    aggregate

                                IndependentEnvironment ->
                                    aggregate
                        )
                        Dict.empty
                        prevCompiledDecls

        blockDeclarationsDirectDependencies : Dict.Dict String (Set.Set String)
        blockDeclarationsDirectDependencies =
            List.foldl
                (\( declName, declExpression ) aggregate ->
                    Dict.insert declName (listUnboundReferencesInExpression declExpression []) aggregate
                )
                Dict.empty
                blockDeclarations

        dependenciesRelations : Dict.Dict String (Set.Set String)
        dependenciesRelations =
            Dict.union availableEmittedDependencies blockDeclarationsDirectDependencies

        forwardedDecls : List ( List String, String )
        forwardedDecls =
            List.map Tuple.first stackBefore.environmentFunctions

        usedAvailableEmittedForInternals : List ( ( List String, String ), EnvironmentFunctionEntry, Pine.Expression )
        usedAvailableEmittedForInternals =
            case forwardedDecls of
                [] ->
                    let
                        contentsDependOnFunctionApplication : Bool
                        contentsDependOnFunctionApplication =
                            List.any
                                (\( _, declExpression ) -> expressionNeedsAdaptiveApplication declExpression)
                                blockDeclarations
                                || List.any expressionNeedsAdaptiveApplication rootDependencies
                    in
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

        emittedImports : List ( ( List String, String ), EnvironmentFunctionEntry, Pine.Expression )
        emittedImports =
            emittedImportsFromRoots
                rootDependencies
                stackBefore
                dependenciesRelations
                blockDeclarations

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
                            case Dict.get declName blockDeclarationsDirectDependencies of
                                Nothing ->
                                    aggregate

                                Just declDirectDeps ->
                                    let
                                        declTransitiveDeps =
                                            getTransitiveDependencies dependenciesRelations declDirectDeps
                                    in
                                    if
                                        List.any
                                            (\depName -> Set.member depName declTransitiveDeps)
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
                , forwarded = forwardedDecls
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
                , forwarded = stackBefore.environmentFunctions
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
                []
            , importedFunctionsToInline = stackBefore.importedFunctionsToInline
            , declarationsDependencies = stackBefore.declarationsDependencies
            , environmentFunctions = environmentFunctions
            , environmentDeconstructions = environmentDeconstructions
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
                            List.indexedMap
                                (\index _ ->
                                    listItemFromIndexExpression_Pine index prevEnvFunctionsExpr
                                )
                                stackBefore.environmentFunctions

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


emittedImportsFromRoots :
    List Expression
    -> EmitStack
    -> Dict.Dict String (Set.Set String)
    -> List ( String, Expression )
    -> List ( ( List String, String ), EnvironmentFunctionEntry, Pine.Expression )
emittedImportsFromRoots rootDependencies emitStack dependenciesRelations blockDecls =
    case emitStack.importedFunctions of
        [] ->
            {-
               Specialize for early exit, because importedFunctions will often be empty,
               as emitDeclarationBlock only emits imports at the root and removes the collection for descendants.
            -}
            []

        importedFunctions ->
            let
                prevCompiledFunctions : List ( String, ( EnvironmentFunctionEntry, Pine.Value ) )
                prevCompiledFunctions =
                    case Common.assocListGet [] importedFunctions of
                        Nothing ->
                            []

                        Just list ->
                            list

                rootDependenciesNames : Set.Set String
                rootDependenciesNames =
                    List.foldl
                        (\depExpr aggregate ->
                            Set.union aggregate (listUnboundReferencesInExpression depExpr [])
                        )
                        Set.empty
                        rootDependencies

                allLocalDependencies : Set.Set String
                allLocalDependencies =
                    getTransitiveDependencies
                        dependenciesRelations
                        rootDependenciesNames

                rootDependenciesImportedNames : Set.Set ( List String, String )
                rootDependenciesImportedNames =
                    List.foldl
                        (\rootDep aggregate ->
                            Set.union
                                (listImportingReferencesInExpression rootDep)
                                aggregate
                        )
                        Set.empty
                        rootDependencies

                allImportedNames : Set.Set ( List String, String )
                allImportedNames =
                    Set.foldl
                        (\depName aggregate ->
                            let
                                addedNames =
                                    case Common.assocListGet depName blockDecls of
                                        Nothing ->
                                            case Common.assocListGet depName prevCompiledFunctions of
                                                Nothing ->
                                                    Set.singleton ( [], depName )

                                                Just ( EnvironmentFunctionEntry _ prevCompiledEnv, _ ) ->
                                                    case prevCompiledEnv of
                                                        LocalEnvironment localDeps ->
                                                            Set.fromList (( [], depName ) :: localDeps)

                                                        _ ->
                                                            Set.singleton ( [], depName )

                                        Just blockDecl ->
                                            listImportingReferencesInExpression blockDecl
                            in
                            Set.union addedNames aggregate
                        )
                        rootDependenciesImportedNames
                        allLocalDependencies
            in
            Set.foldl
                (\( moduleName, declName ) aggregate ->
                    case Common.assocListGet moduleName importedFunctions of
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


{-| Searches the tree of subexpressions for any that might require adaptive application.
-}
expressionNeedsAdaptiveApplication : Expression -> Bool
expressionNeedsAdaptiveApplication expression =
    {-
       This function seems brittle because it needs to match the behavior of others, such as emitFunctionApplication.
       Changing something in the selection for inlining might require changes here as well.
       Perhaps it is better to somehow reuse the same logic here.
    -}
    case expression of
        LiteralExpression _ ->
            False

        ListExpression list ->
            List.any expressionNeedsAdaptiveApplication list

        KernelApplicationExpression argument _ ->
            expressionNeedsAdaptiveApplication argument

        ConditionalExpression condition falseBranch trueBranch ->
            expressionNeedsAdaptiveApplication condition
                || expressionNeedsAdaptiveApplication trueBranch
                || expressionNeedsAdaptiveApplication falseBranch

        FunctionExpression _ functionBody ->
            expressionNeedsAdaptiveApplication functionBody

        FunctionApplicationExpression funcExpr args ->
            case funcExpr of
                LiteralExpression _ ->
                    {-
                       Whether that function should be inlined or not, in any case we should not need
                       to supply the generic function for adaptive application.
                    -}
                    List.any expressionNeedsAdaptiveApplication args

                _ ->
                    case args of
                        [] ->
                            expressionNeedsAdaptiveApplication funcExpr

                        _ ->
                            True

        DeclarationBlockExpression declarations innerExpression ->
            List.any
                (\( _, decl ) -> expressionNeedsAdaptiveApplication decl)
                declarations
                || expressionNeedsAdaptiveApplication innerExpression

        ReferenceExpression _ _ ->
            False

        StringTagExpression _ tagged ->
            expressionNeedsAdaptiveApplication tagged

        PineFunctionApplicationExpression _ argument ->
            expressionNeedsAdaptiveApplication argument


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
                        (String.join ""
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


listTransitiveDependenciesOfExpression : EmitStack -> Expression -> Set.Set String
listTransitiveDependenciesOfExpression dependenciesRelations expression =
    getTransitiveDependencies dependenciesRelations.declarationsDependencies
        (listUnboundReferencesInExpression expression [])


listUnboundReferencesInExpression : Expression -> List String -> Set.Set String
listUnboundReferencesInExpression expression boundNames =
    case expression of
        LiteralExpression _ ->
            Set.empty

        ListExpression list ->
            List.foldl
                (\item aggregate ->
                    Set.union (listUnboundReferencesInExpression item boundNames) aggregate
                )
                Set.empty
                list

        KernelApplicationExpression argument _ ->
            listUnboundReferencesInExpression argument boundNames

        ConditionalExpression condition falseBranch trueBranch ->
            Set.union (listUnboundReferencesInExpression falseBranch boundNames)
                (Set.union (listUnboundReferencesInExpression trueBranch boundNames)
                    (listUnboundReferencesInExpression condition boundNames)
                )

        ReferenceExpression moduleName reference ->
            case moduleName of
                [] ->
                    if List.member reference boundNames then
                        Set.empty

                    else
                        Set.singleton reference

                _ ->
                    Set.empty

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
            List.foldl
                (\argument aggregate ->
                    Set.union (listUnboundReferencesInExpression argument boundNames) aggregate
                )
                (listUnboundReferencesInExpression functionExpression boundNames)
                arguments

        DeclarationBlockExpression declarations innerExpression ->
            let
                declarationsNames : List String
                declarationsNames =
                    List.map Tuple.first declarations

                newBoundNames =
                    List.concat [ boundNames, declarationsNames ]
            in
            List.foldl
                (\( _, decl ) aggregate ->
                    Set.union (listUnboundReferencesInExpression decl newBoundNames) aggregate
                )
                (listUnboundReferencesInExpression innerExpression newBoundNames)
                declarations

        StringTagExpression _ tagged ->
            listUnboundReferencesInExpression tagged boundNames

        PineFunctionApplicationExpression _ argument ->
            listUnboundReferencesInExpression argument boundNames


listImportingReferencesInExpression : Expression -> Set.Set ( List String, String )
listImportingReferencesInExpression expression =
    case expression of
        LiteralExpression _ ->
            Set.empty

        ListExpression list ->
            List.foldl
                (\item aggregate ->
                    Set.union (listImportingReferencesInExpression item) aggregate
                )
                Set.empty
                list

        KernelApplicationExpression argument _ ->
            listImportingReferencesInExpression argument

        ConditionalExpression condition falseBranch trueBranch ->
            Set.union (listImportingReferencesInExpression falseBranch)
                (Set.union (listImportingReferencesInExpression trueBranch)
                    (listImportingReferencesInExpression condition)
                )

        ReferenceExpression moduleName reference ->
            case moduleName of
                [] ->
                    Set.empty

                _ ->
                    Set.singleton ( moduleName, reference )

        FunctionExpression _ functionBody ->
            listImportingReferencesInExpression
                functionBody

        FunctionApplicationExpression functionExpression arguments ->
            List.foldl
                (\argument aggregate ->
                    Set.union (listImportingReferencesInExpression argument) aggregate
                )
                (listImportingReferencesInExpression functionExpression)
                arguments

        DeclarationBlockExpression declarations innerExpression ->
            List.foldl
                (\( _, decl ) aggregate ->
                    Set.union (listImportingReferencesInExpression decl) aggregate
                )
                (listImportingReferencesInExpression innerExpression)
                declarations

        StringTagExpression _ tagged ->
            listImportingReferencesInExpression tagged

        PineFunctionApplicationExpression _ argument ->
            listImportingReferencesInExpression argument


getTransitiveDependencies : Dict.Dict String (Set.Set String) -> Set.Set String -> Set.Set String
getTransitiveDependencies dependenciesDependencies current =
    let
        stepResult =
            mergeDependenciesStep dependenciesDependencies current
    in
    if Set.size stepResult == Set.size current then
        stepResult

    else
        getTransitiveDependencies dependenciesDependencies stepResult


mergeDependenciesStep : Dict.Dict String (Set.Set String) -> Set.Set String -> Set.Set String
mergeDependenciesStep dependenciesDependencies references =
    Set.foldl
        (\reference aggregate ->
            case Dict.get reference dependenciesDependencies of
                Nothing ->
                    aggregate

                Just dependencies ->
                    Set.union dependencies aggregate
        )
        references
        references


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
                    ( emittedArgument
                    , pineFunctionExpression
                        |> Pine.encodeExpressionAsValue
                        |> Pine.LiteralExpression
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
                                    funcBodyDeps : Set.Set String
                                    funcBodyDeps =
                                        listTransitiveDependenciesOfExpression compilation funcBody

                                    closureCaptures : List ( String, EnvironmentDeconstructionEntry )
                                    closureCaptures =
                                        List.foldl
                                            (\( declName, deconstruction ) aggregate ->
                                                if Set.member declName funcBodyDeps then
                                                    ( declName, deconstruction ) :: aggregate

                                                else
                                                    aggregate
                                            )
                                            []
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
                                                (Pine.ListExpression
                                                    [ prevEnvFunctionsExpr
                                                    , Pine.ListExpression argumentsPineWithClosure
                                                    ]
                                                )
                                                (Pine.LiteralExpression
                                                    (Pine.encodeExpressionAsValue funcBodyEmitted)
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


emitFunctionApplicationPine : EmitStack -> List Pine.Expression -> Pine.Expression -> Result String Pine.Expression
emitFunctionApplicationPine emitStack arguments functionExpressionPine =
    let
        genericPartialApplication () =
            partialApplicationExpressionFromListOfArguments
                arguments
                emitStack
                functionExpressionPine
    in
    if not (pineExpressionIsIndependent functionExpressionPine) then
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
                                    (Pine.ListExpression
                                        [ importedGetEnvFunctionsExpression
                                        , Pine.ListExpression arguments
                                        ]
                                    )
                                    importedGetFunctionExpr

                             else
                                partialApplicationExpressionFromListOfArguments
                                    arguments
                                    compilation
                                    funcRecordLessTag
                            )
                        )

                LocalEnvironment localEnvExpectedDecls ->
                    let
                        currentEnv : List ( List String, String )
                        currentEnv =
                            List.map Tuple.first compilation.environmentFunctions

                        currentEnvCoversExpected : Bool
                        currentEnvCoversExpected =
                            List.take (List.length localEnvExpectedDecls) currentEnv == localEnvExpectedDecls

                        buildEnvironmentRecursive :
                            List Pine.Expression
                            -> List ( List String, String )
                            -> Result String Pine.Expression
                        buildEnvironmentRecursive alreadyMapped remainingToBeMapped =
                            case remainingToBeMapped of
                                [] ->
                                    Ok (Pine.ListExpression alreadyMapped)

                                nextExpectedFunctionName :: remainingExpectedFunctions ->
                                    case currentEnvironmentFunctionEntryFromName nextExpectedFunctionName of
                                        Nothing ->
                                            let
                                                ( expectModuleName, expectDeclName ) =
                                                    nextExpectedFunctionName
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
                                                remainingExpectedFunctions

                        buildExpectedEnvironmentResult =
                            if currentEnvCoversExpected then
                                case localEnvExpectedDecls of
                                    [] ->
                                        Ok (Pine.ListExpression [])

                                    _ ->
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
                                            (Pine.ListExpression
                                                [ expectedEnvironment
                                                , Pine.ListExpression arguments
                                                ]
                                            )
                                            getFunctionExpression

                                     else
                                        partialApplicationExpressionFromListOfArguments
                                            arguments
                                            compilation
                                            (case funcParamCount of
                                                0 ->
                                                    Pine.ParseAndEvalExpression
                                                        (Pine.ListExpression
                                                            [ expectedEnvironment
                                                            , Pine.ListExpression []
                                                            ]
                                                        )
                                                        getFunctionExpression

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
        (Pine.ListExpression
            [ getEnvFunctionsExpression
            , Pine.ListExpression []
            ]
        )
        getFunctionInnerExpression


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
                (Pine.ListExpression
                    [ applicationFunctionExpr
                    , config.function
                    , Pine.ListExpression config.arguments
                    ]
                )
                applicationFunctionExpr


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
                    ( nextArgumentLocalExpression
                    , functionLocalExpression
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
                            (Pine.ListExpression
                                [ previouslyCollectedArguments
                                , Pine.ListExpression [ nextArgumentLocalExpression ]
                                ]
                            )
                            "concat"

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
                        (Pine.ListExpression
                            [ environmentFunctions
                            , collectedArguments
                            ]
                        )
                        innerFunction
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
            (Pine.ListExpression
                [ selfFunctionLocalExpression
                , applyNextExpression
                , listSkipExpression_Pine 1 remainingArgumentsLocalExpression
                ]
            )
            selfFunctionLocalExpression
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
                    if functionTag == Pine.stringAsValue_Function then
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
attemptReduceParseAndEvalExpressionRecursiveWithDefaultDepth ( origEnvExpr, origExprExpr ) =
    let
        sizeBeforeReduction =
            countPineExpressionSize estimatePineValueSize origEnvExpr
                + countPineExpressionSize estimatePineValueSize origExprExpr

        reductionMaxDepth =
            if sizeBeforeReduction < 10 * 1000 then
                2

            else
                1
    in
    attemptReduceParseAndEvalExpressionRecursive
        reductionMaxDepth
        ( origEnvExpr, origExprExpr )


attemptReduceParseAndEvalExpressionRecursive :
    Int
    -> ( Pine.Expression, Pine.Expression )
    -> Pine.Expression
attemptReduceParseAndEvalExpressionRecursive maxDepth ( origEnvExpr, origExprExpr ) =
    let
        default =
            Pine.ParseAndEvalExpression origEnvExpr origExprExpr
    in
    if maxDepth < 1 then
        default

    else
        case searchReductionForParseAndEvalExpression ( origEnvExpr, origExprExpr ) of
            Nothing ->
                default

            Just reduced ->
                case reduced of
                    Pine.ParseAndEvalExpression reducedEnvExpr reducedExprExpr ->
                        attemptReduceParseAndEvalExpressionRecursive
                            (maxDepth - 1)
                            ( reducedEnvExpr, reducedExprExpr )

                    _ ->
                        reduced


searchReductionForParseAndEvalExpression :
    ( Pine.Expression, Pine.Expression )
    -> Maybe Pine.Expression
searchReductionForParseAndEvalExpression ( origEnvExpr, origExprExpr ) =
    if pineExpressionIsIndependent origExprExpr then
        case Pine.evaluateExpression Pine.emptyEvalEnvironment origExprExpr of
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

        Pine.KernelApplicationExpression rootArgument rootFunctionName ->
            case rootFunctionName of
                "list_head" ->
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

                Pine.ParseAndEvalExpression envExpr exprExpr ->
                    let
                        ( envTransform, envInspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement envExpr

                        ( exprTransform, exprInspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement exprExpr
                    in
                    ( Pine.ParseAndEvalExpression
                        envTransform
                        exprTransform
                    , { referencesOriginalEnvironment =
                            exprInspect.referencesOriginalEnvironment
                                || envInspect.referencesOriginalEnvironment
                      }
                    )

                Pine.KernelApplicationExpression argumentOrig functionName ->
                    let
                        ( argument, inspect ) =
                            transformPineExpressionWithOptionalReplacement findReplacement argumentOrig
                    in
                    ( Pine.KernelApplicationExpression argument functionName
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

        KernelApplicationExpression argument _ ->
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

        Pine.ParseAndEvalExpression envExpr exprExpr ->
            pineExpressionIsIndependent envExpr
                && pineExpressionIsIndependent exprExpr

        Pine.KernelApplicationExpression argument _ ->
            pineExpressionIsIndependent argument

        Pine.ConditionalExpression condition falseBranch trueBranch ->
            pineExpressionIsIndependent condition
                && pineExpressionIsIndependent falseBranch
                && pineExpressionIsIndependent trueBranch

        Pine.EnvironmentExpression ->
            False

        Pine.StringTagExpression _ tagged ->
            pineExpressionIsIndependent tagged


listItemFromIndexExpression : Int -> Expression -> Expression
listItemFromIndexExpression itemIndex listExpression =
    pineKernel_ListHead (listSkipExpression itemIndex listExpression)


countListElementsExpression : Expression -> Expression
countListElementsExpression sequenceExpression =
    KernelApplicationExpression
        sequenceExpression
        "length"


pineKernel_ListHead : Expression -> Expression
pineKernel_ListHead listExpression =
    KernelApplicationExpression
        listExpression
        "list_head"


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
        (ListExpression list)
        "equal"


applyKernelFunctionWithTwoArguments : String -> Expression -> Expression -> Expression
applyKernelFunctionWithTwoArguments kernelFunctionName argA argB =
    KernelApplicationExpression
        (ListExpression [ argA, argB ])
        kernelFunctionName


countListElementsExpression_Pine : Pine.Expression -> Pine.Expression
countListElementsExpression_Pine sequenceExpression =
    Pine.KernelApplicationExpression
        sequenceExpression
        "length"


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
            (Pine.LiteralExpression (Pine.valueFromInt numberToDrop))
            listExpression


pineKernel_ListHead_Pine : Pine.Expression -> Pine.Expression
pineKernel_ListHead_Pine listExpression =
    Pine.KernelApplicationExpression
        listExpression
        "list_head"


equalCondition_Pine : List Pine.Expression -> Pine.Expression
equalCondition_Pine list =
    Pine.KernelApplicationExpression
        (Pine.ListExpression list)
        "equal"


applyKernelFunctionWithTwoArguments_Pine : String -> Pine.Expression -> Pine.Expression -> Pine.Expression
applyKernelFunctionWithTwoArguments_Pine kernelFunctionName argA argB =
    Pine.KernelApplicationExpression
        (Pine.ListExpression [ argA, argB ])
        kernelFunctionName


countPineExpressionSize : (Pine.Value -> Int) -> Pine.Expression -> Int
countPineExpressionSize countValueSize expression =
    case expression of
        Pine.LiteralExpression literal ->
            countValueSize literal

        Pine.ListExpression list ->
            List.foldl (\item sum -> sum + countPineExpressionSize countValueSize item)
                1
                list

        Pine.ParseAndEvalExpression envExpr exprExpr ->
            countPineExpressionSize countValueSize envExpr
                + countPineExpressionSize countValueSize exprExpr

        Pine.KernelApplicationExpression argument _ ->
            2 + countPineExpressionSize countValueSize argument

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
    case value of
        Pine.BlobValue blob ->
            10 + List.length blob

        Pine.ListValue list ->
            10 + estimatePineListValueSizeHelper 0 list


estimatePineListValueSizeHelper : Int -> List Pine.Value -> Int
estimatePineListValueSizeHelper accumulated list =
    -- Reduce stack depths by matching the most common cases with few elements inline.
    case list of
        [] ->
            accumulated

        [ first ] ->
            accumulated
                + estimatePineValueSize first

        [ first, second ] ->
            accumulated
                + estimatePineValueSize first
                + estimatePineValueSize second

        first :: second :: third :: remaining ->
            estimatePineListValueSizeHelper
                (accumulated
                    + estimatePineValueSize first
                    + estimatePineValueSize second
                    + estimatePineValueSize third
                )
                remaining
