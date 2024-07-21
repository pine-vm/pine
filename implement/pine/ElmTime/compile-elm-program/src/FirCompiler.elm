module FirCompiler exposing
    ( DeclBlockAdditionalDeps(..)
    , DeclBlockClosureCaptures(..)
    , DeclBlockFunctionEntry(..)
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
    , listDirectDependenciesOfExpression
    , listFunctionAppExpressions
    , listItemFromIndexExpression
    , listItemFromIndexExpression_Pine
    , listSkipExpression
    , listSkipExpression_Pine
    , listTransitiveDependenciesOfExpression
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
    | ReferenceExpression String
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
    { importedFunctions : List ( String, ( EnvironmentFunctionEntry, Pine.Value ) )
    , importedFunctionsToInline : List ( String, Pine.Value )
    , declarationsDependencies : Dict.Dict String (Set.Set String)

    -- The functions in the first item in the environment list
    , environmentFunctions : List ( String, EnvironmentFunctionEntry )

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
        (List String)
    | ImportedEnvironment
        -- Path to the tagged function record relative to the entry in the current environment.
        (List Deconstruction)
    | IndependentEnvironment


type alias EnvironmentDeconstructionEntry =
    List Deconstruction


type DeclBlockClosureCaptures
    = DeclBlockClosureCaptures (List ( String, EnvironmentDeconstructionEntry ))


type DeclBlockAdditionalDeps
    = DeclBlockAdditionalDeps (List Expression)


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

        ReferenceExpression localReference ->
            emitReferenceExpression localReference stack

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
                            Dict.insert declName (listDirectDependenciesOfExpression declExpression) aggregate
                        )
                        stackBeforeAddingDeps.declarationsDependencies
                        blockDeclarations
            }

        mainExpressionOuterDependencies : Set.Set String
        mainExpressionOuterDependencies =
            listTransitiveDependenciesOfExpression stackBefore mainExpression

        remainingOuterDependencies : Set.Set String
        remainingOuterDependencies =
            -- Not supporting shadowing at the moment: Filter out every name we already have from a parent scope.
            List.foldl
                (\( functionName, _ ) aggregate ->
                    Set.remove functionName aggregate
                )
                mainExpressionOuterDependencies
                stackBefore.environmentFunctions

        usedBlockDeclarationsAndImports : List ( String, Expression )
        usedBlockDeclarationsAndImports =
            Set.foldl
                (\declName aggregate ->
                    case Common.assocListGet declName blockDeclarations of
                        Just declExpression ->
                            ( declName, declExpression ) :: aggregate

                        Nothing ->
                            case Common.assocListGet declName stackBeforeAddingDeps.importedFunctions of
                                Nothing ->
                                    aggregate

                                Just ( _, importedVal ) ->
                                    ( declName, LiteralExpression importedVal ) :: aggregate
                )
                []
                remainingOuterDependencies

        mainExpressionAsFunction : DeclBlockFunctionEntry
        mainExpressionAsFunction =
            parseFunctionParameters mainExpression

        (DeclBlockFunctionEntry mainExprParams mainExprInnerExpr) =
            mainExpressionAsFunction

        closureCaptures : List ( String, EnvironmentDeconstructionEntry )
        closureCaptures =
            List.foldl
                (\( declName, deconstruction ) aggregate ->
                    if Set.member declName mainExpressionOuterDependencies then
                        ( declName, deconstruction ) :: aggregate

                    else
                        aggregate
                )
                []
                stackBefore.environmentDeconstructions
    in
    if mainExprParams == [] && usedBlockDeclarationsAndImports == [] then
        emitExpression stackBeforeAddingDeps mainExprInnerExpr

    else
        case
            emitDeclarationBlock
                stackBefore
                usedBlockDeclarationsAndImports
                (DeclBlockClosureCaptures closureCaptures)
                (DeclBlockAdditionalDeps [ mainExpression ])
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


emitDeclarationBlock :
    EmitStack
    -> List ( String, Expression )
    -> DeclBlockClosureCaptures
    -> DeclBlockAdditionalDeps
    -> Result String ( EmitStack, EmitDeclarationBlockResult )
emitDeclarationBlock stackBefore blockDeclarations (DeclBlockClosureCaptures configClosureCaptures) (DeclBlockAdditionalDeps additionalDeps) =
    let
        availableEmittedDependencies : Dict.Dict String (Set.Set String)
        availableEmittedDependencies =
            List.foldl
                (\( functionName, ( EnvironmentFunctionEntry _ expectedEnvironment, _ ) ) aggregate ->
                    case expectedEnvironment of
                        LocalEnvironment localEnvExpectedDecls ->
                            Dict.insert
                                functionName
                                (Set.fromList localEnvExpectedDecls)
                                aggregate

                        ImportedEnvironment _ ->
                            aggregate

                        IndependentEnvironment ->
                            aggregate
                )
                Dict.empty
                stackBefore.importedFunctions

        blockDeclarationsDirectDependencies : Dict.Dict String (Set.Set String)
        blockDeclarationsDirectDependencies =
            List.foldl
                (\( declName, declExpression ) aggregate ->
                    Dict.insert declName (listDirectDependenciesOfExpression declExpression) aggregate
                )
                Dict.empty
                blockDeclarations

        dependenciesRelations : Dict.Dict String (Set.Set String)
        dependenciesRelations =
            Dict.union availableEmittedDependencies blockDeclarationsDirectDependencies

        blockDeclarationsTransitiveDependencies : Dict.Dict String (Set.Set String)
        blockDeclarationsTransitiveDependencies =
            Dict.map
                (\_ declDirectDeps -> getTransitiveDependencies dependenciesRelations declDirectDeps)
                blockDeclarationsDirectDependencies

        additionalImports : Set.Set String
        additionalImports =
            List.foldl
                (\depExpr aggregate ->
                    Set.union aggregate (listTransitiveDependenciesOfExpression stackBefore depExpr)
                )
                Set.empty
                additionalDeps

        allDependencies : Set.Set String
        allDependencies =
            Set.union
                additionalImports
                (getTransitiveDependencies
                    dependenciesRelations
                    (Dict.foldl (\_ dependencies -> Set.union dependencies)
                        Set.empty
                        blockDeclarationsDirectDependencies
                    )
                )

        stackBeforeAvailableDeclarations : Set.Set String
        stackBeforeAvailableDeclarations =
            List.foldl
                (\( functionName, _ ) aggregate -> Set.insert functionName aggregate)
                (List.foldl
                    (\( declName, _ ) aggregate -> Set.insert declName aggregate)
                    Set.empty
                    stackBefore.environmentDeconstructions
                )
                stackBefore.environmentFunctions

        usedAvailableEmitted : List ( String, EnvironmentFunctionEntry, Pine.Expression )
        usedAvailableEmitted =
            Set.foldl
                (\depName aggregate ->
                    case Common.assocListGet depName stackBefore.importedFunctions of
                        Nothing ->
                            aggregate

                        Just ( availableEmitted, emittedValue ) ->
                            if Set.member depName stackBeforeAvailableDeclarations then
                                aggregate

                            else
                                ( depName, availableEmitted, Pine.LiteralExpression emittedValue ) :: aggregate
                )
                []
                allDependencies

        usedAvailableEmittedNames : Set.Set String
        usedAvailableEmittedNames =
            List.foldl
                (\( functionName, _, _ ) aggregate -> Set.insert functionName aggregate)
                Set.empty
                usedAvailableEmitted

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

        prefixEnvironmentFunctions : List ( String, EnvironmentFunctionEntry )
        prefixEnvironmentFunctions =
            List.map
                (\( functionName, functionEntry, _ ) -> ( functionName, functionEntry ))
                usedAvailableEmitted

        prependedEnvFunctionsExpressions : List Pine.Expression
        prependedEnvFunctionsExpressions =
            List.map
                (\( _, _, emittedExpr ) -> emittedExpr)
                usedAvailableEmitted

        forwardedDecls : List String
        forwardedDecls =
            List.map
                (\( functionName, _ ) -> functionName)
                stackBefore.environmentFunctions

        contentsDependOnFunctionApplication : Bool
        contentsDependOnFunctionApplication =
            List.any
                (\( _, declExpression ) -> expressionNeedsAdaptiveApplication declExpression)
                blockDeclarations
                || List.any expressionNeedsAdaptiveApplication additionalDeps

        closureCapturesForInternals : List ( String, Expression )
        closureCapturesForInternals =
            if List.member environmentFunctionPartialApplicationName forwardedDecls then
                []

            else if not contentsDependOnFunctionApplication then
                []

            else
                [ ( environmentFunctionPartialApplicationName
                  , if Set.member environmentFunctionPartialApplicationName stackBeforeAvailableDeclarations then
                        ReferenceExpression environmentFunctionPartialApplicationName

                    else
                        LiteralExpression (adaptivePartialApplicationRecursiveValue ())
                  )
                ]

        closureCapturesForBlockDecls : List ( String, Expression )
        closureCapturesForBlockDecls =
            {-
               To avoid repeated evaluation of declarations without parameters from a let-block at runtime,
               Map them to closure captures list so these are only evaluated once.
            -}
            List.foldl
                (\( declName, DeclBlockFunctionEntry asFunctionParams asFunctionInnerExpr ) aggregate ->
                    if asFunctionParams /= [] then
                        aggregate

                    else
                        case Dict.get declName blockDeclarationsTransitiveDependencies of
                            Nothing ->
                                aggregate

                            Just declDependencies ->
                                if
                                    not (Set.member declName declDependencies)
                                        && (Set.diff declDependencies stackBeforeAvailableDeclarations == Set.empty)
                                then
                                    if Set.member declName usedAvailableEmittedNames then
                                        aggregate

                                    else
                                        ( declName, asFunctionInnerExpr ) :: aggregate

                                else
                                    aggregate
                )
                []
                allBlockDeclarationsAsFunctions

        blockDeclarationsAsFunctionsLessClosure : List ( String, DeclBlockFunctionEntry )
        blockDeclarationsAsFunctionsLessClosure =
            List.filter
                (\( declName, _ ) ->
                    not (List.any (\( name, _ ) -> name == declName) closureCapturesForBlockDecls)
                )
                allBlockDeclarationsAsFunctions

        closureCaptures : List ( String, ClosureCapture )
        closureCaptures =
            List.concat
                [ List.map (Tuple.mapSecond DeconstructionCapture)
                    configClosureCaptures
                , List.map (Tuple.mapSecond ExpressionCapture)
                    (closureCapturesForInternals ++ closureCapturesForBlockDecls)
                ]

        newEnvironmentFunctionsNames : List String
        newEnvironmentFunctionsNames =
            composeEnvironmentFunctions
                { prefix =
                    List.map
                        (\( functionName, _, _ ) -> functionName)
                        usedAvailableEmitted
                , forwarded = forwardedDecls
                , appendedFromDecls = List.map Tuple.first blockDeclarationsAsFunctionsLessClosure
                , appendedFromClosureCaptures = List.map Tuple.first closureCaptures
                }

        newEnvironmentFunctionsFromDecls : List ( String, EnvironmentFunctionEntry )
        newEnvironmentFunctionsFromDecls =
            List.map
                (\( functionName, DeclBlockFunctionEntry functionEntryParams _ ) ->
                    ( functionName
                    , EnvironmentFunctionEntry
                        (List.length functionEntryParams)
                        (LocalEnvironment newEnvironmentFunctionsNames)
                    )
                )
                blockDeclarationsAsFunctionsLessClosure

        newEnvironmentFunctionsFromClosureCaptures : List ( String, EnvironmentFunctionEntry )
        newEnvironmentFunctionsFromClosureCaptures =
            List.map
                (\( captureName, _ ) ->
                    ( captureName
                    , EnvironmentFunctionEntry 0 IndependentEnvironment
                    )
                )
                closureCaptures

        environmentFunctions : List ( String, EnvironmentFunctionEntry )
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
                (emitFunctionStack (closureParameterFromParameters functionEntryParams))
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
                    expressionNeedsAdaptiveApplication funcExpr || (args /= [])

        DeclarationBlockExpression declarations innerExpression ->
            List.foldl
                (\( _, decl ) aggregate ->
                    expressionNeedsAdaptiveApplication decl
                        || aggregate
                )
                (expressionNeedsAdaptiveApplication innerExpression)
                declarations

        ReferenceExpression _ ->
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
recursionDomainsFromDeclarationDependencies : Dict.Dict String (Set.Set String) -> List (Set.Set String)
recursionDomainsFromDeclarationDependencies declarationDependencies =
    let
        integrateDecl declName declDependencies recursionDomains =
            let
                -- Inserts the new domain into the list of domains at the position where the none of the following domains depend on it.
                insertDomainRecursive :
                    Set.Set String
                    -> List (Set.Set String)
                    -> List (Set.Set String)
                    -> List (Set.Set String)
                insertDomainRecursive domainToInsert skipped following =
                    case following of
                        [] ->
                            skipped ++ [ domainToInsert ]

                        next :: rest ->
                            let
                                allCurrentAndFollowing : Set.Set String
                                allCurrentAndFollowing =
                                    List.foldl Set.union next rest

                                dependingOnAnyCurrentOrFollowing : Bool
                                dependingOnAnyCurrentOrFollowing =
                                    not (Set.isEmpty (Set.intersect declDependencies allCurrentAndFollowing))

                                allDependenciesOfNext : Set.Set String
                                allDependenciesOfNext =
                                    Set.foldl
                                        (\nextDeclName aggregate ->
                                            case Dict.get nextDeclName declarationDependencies of
                                                Nothing ->
                                                    aggregate

                                                Just nextDeclDependencies ->
                                                    Set.union nextDeclDependencies aggregate
                                        )
                                        Set.empty
                                        next

                                nextDependingOnNewDomain : Bool
                                nextDependingOnNewDomain =
                                    Set.member declName allDependenciesOfNext
                            in
                            if dependingOnAnyCurrentOrFollowing then
                                if nextDependingOnNewDomain then
                                    -- Merge the new domain into the current domain
                                    List.concat
                                        [ skipped, [ Set.union domainToInsert next ], rest ]

                                else
                                    insertDomainRecursive domainToInsert (skipped ++ [ next ]) rest

                            else
                                List.concat
                                    [ skipped, [ domainToInsert ], following ]
            in
            insertDomainRecursive (Set.singleton declName) [] recursionDomains
    in
    Dict.foldl integrateDecl [] declarationDependencies


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


emitReferenceExpression : String -> EmitStack -> Result String Pine.Expression
emitReferenceExpression name compilation =
    {-
       Prioritize environmentDeconstructions before environmentFunctions here to
       support shadowing for function parameters.
       A source language like Elm does not support shadowing anyway, but the current
       implementation of the Elm compiler sometimes lowers to Fir code that introduces declarations,
       which can result in shadowing when nested.
       An example is the `pseudoParamName` in `compileElmSyntaxCaseBlock`
    -}
    case Common.assocListGet name compilation.environmentDeconstructions of
        Just deconstruction ->
            Ok
                (pineExpressionForDeconstructions deconstruction
                    (listItemFromIndexExpression_Pine 1 Pine.environmentExpr)
                )

        Nothing ->
            case Common.assocListGet name compilation.importedFunctionsToInline of
                Just importedFunction ->
                    Ok (Pine.LiteralExpression importedFunction)

                Nothing ->
                    case emitApplyFunctionFromCurrentEnvironment compilation name [] of
                        Nothing ->
                            Err
                                (String.join ""
                                    [ "Failed referencing '"
                                    , name
                                    , "'. "
                                    , String.fromInt (List.length compilation.environmentDeconstructions)
                                    , " deconstructions in scope: "
                                    , String.join ", " (List.map Tuple.first compilation.environmentDeconstructions)
                                    , ". "
                                    , String.fromInt (List.length compilation.environmentFunctions)
                                    , " functions in scope: "
                                    , String.join ", " (List.map Tuple.first compilation.environmentFunctions)
                                    ]
                                )

                        Just (Err err) ->
                            Err ("Failed emitting reference as function application: " ++ err)

                        Just (Ok functionApplicationOk) ->
                            Ok functionApplicationOk


listTransitiveDependenciesOfExpression : EmitStack -> Expression -> Set.Set String
listTransitiveDependenciesOfExpression dependenciesRelations expression =
    getTransitiveDependencies dependenciesRelations.declarationsDependencies
        (listDirectDependenciesOfExpression expression)


listDirectDependenciesOfExpression : Expression -> Set.Set String
listDirectDependenciesOfExpression expression =
    case expression of
        LiteralExpression _ ->
            Set.empty

        ListExpression list ->
            List.foldl
                (\item aggregate -> Set.union (listDirectDependenciesOfExpression item) aggregate)
                Set.empty
                list

        KernelApplicationExpression argument _ ->
            listDirectDependenciesOfExpression argument

        ConditionalExpression condition falseBranch trueBranch ->
            Set.union (listDirectDependenciesOfExpression falseBranch)
                (Set.union (listDirectDependenciesOfExpression trueBranch)
                    (listDirectDependenciesOfExpression condition)
                )

        ReferenceExpression reference ->
            Set.singleton reference

        FunctionExpression functionParam functionBody ->
            let
                functionBodyDependencies =
                    listDirectDependenciesOfExpression functionBody

                functionParamNames : List String
                functionParamNames =
                    List.concatMap
                        (\param -> List.map Tuple.first param)
                        functionParam
            in
            List.foldl
                (\paramName aggregate -> Set.remove paramName aggregate)
                functionBodyDependencies
                functionParamNames

        FunctionApplicationExpression functionExpression arguments ->
            List.foldl
                (\argument aggregate -> Set.union (listDirectDependenciesOfExpression argument) aggregate)
                (listDirectDependenciesOfExpression functionExpression)
                arguments

        DeclarationBlockExpression declarations innerExpression ->
            let
                innerDependencies : Set.Set String
                innerDependencies =
                    List.foldl
                        (\( _, decl ) aggregate -> Set.union (listDirectDependenciesOfExpression decl) aggregate)
                        (listDirectDependenciesOfExpression innerExpression)
                        declarations
            in
            List.foldl
                (\( declName, _ ) -> Set.remove declName)
                innerDependencies
                declarations

        StringTagExpression _ tagged ->
            listDirectDependenciesOfExpression tagged

        PineFunctionApplicationExpression _ argument ->
            listDirectDependenciesOfExpression argument


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


closureParameterFromParameters : List FunctionParam -> FunctionParam
closureParameterFromParameters parameters =
    List.concat
        (List.indexedMap
            (\paramIndex paramDeconstructions ->
                List.map
                    (\( paramName, paramDecons ) ->
                        ( paramName, ListItemDeconstruction paramIndex :: paramDecons )
                    )
                    paramDeconstructions
            )
            parameters
        )


emitFunctionApplication : Expression -> List Expression -> EmitStack -> Result String Pine.Expression
emitFunctionApplication functionExpression arguments compilation =
    if arguments == [] then
        emitExpression compilation functionExpression

    else
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

                                envFunctionsFromClosureCaptures : List ( String, EnvironmentFunctionEntry )
                                envFunctionsFromClosureCaptures =
                                    List.map
                                        (\( captureName, _ ) ->
                                            ( captureName
                                            , EnvironmentFunctionEntry 0 IndependentEnvironment
                                            )
                                        )
                                        closureCaptures

                                appendedEnvFunctionsExpressions : List Pine.Expression
                                appendedEnvFunctionsExpressions =
                                    List.map
                                        (\( _, deconstruction ) ->
                                            pineExpressionForDeconstructions
                                                deconstruction
                                                (listItemFromIndexExpression_Pine 1 Pine.environmentExpr)
                                        )
                                        closureCaptures

                                environmentFunctions : List ( String, EnvironmentFunctionEntry )
                                environmentFunctions =
                                    List.concat
                                        [ compilation.environmentFunctions, envFunctionsFromClosureCaptures ]

                                newEmitStack =
                                    { compilation
                                        | environmentDeconstructions = closureParameterFromParameters params
                                        , environmentFunctions = environmentFunctions
                                    }

                                prevEnvFunctionsExpr : Pine.Expression
                                prevEnvFunctionsExpr =
                                    listItemFromIndexExpression_Pine 0 Pine.environmentExpr

                                forwardedItems : List Pine.Expression
                                forwardedItems =
                                    List.indexedMap
                                        (\index _ ->
                                            listItemFromIndexExpression_Pine index prevEnvFunctionsExpr
                                        )
                                        compilation.environmentFunctions

                                envFunctionsExpr : Pine.Expression
                                envFunctionsExpr =
                                    Pine.ListExpression
                                        (List.concat
                                            [ forwardedItems
                                            , appendedEnvFunctionsExpressions
                                            ]
                                        )
                            in
                            case emitExpression newEmitStack funcBody of
                                Err err ->
                                    Err ("Failed emitting function body: " ++ err)

                                Ok funcBodyEmitted ->
                                    Ok
                                        (Pine.ParseAndEvalExpression
                                            (Pine.ListExpression
                                                [ envFunctionsExpr
                                                , Pine.ListExpression argumentsPine
                                                ]
                                            )
                                            (Pine.LiteralExpression
                                                (Pine.encodeExpressionAsValue funcBodyEmitted)
                                            )
                                        )

                    ReferenceExpression functionName ->
                        case emitApplyFunctionFromCurrentEnvironment compilation functionName argumentsPine of
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
    -> String
    -> List Pine.Expression
    -> Maybe (Result String Pine.Expression)
emitApplyFunctionFromCurrentEnvironment compilation functionName arguments =
    let
        currentEnvironmentFunctionEntryFromName : String -> Maybe ( Int, EnvironmentFunctionEntry )
        currentEnvironmentFunctionEntryFromName name =
            Common.assocListGetWithIndex name compilation.environmentFunctions
    in
    case currentEnvironmentFunctionEntryFromName functionName of
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
                                    (Pine.ParseAndEvalExpression
                                        (Pine.ListExpression
                                            [ Pine.ListExpression []
                                            , Pine.ListExpression arguments
                                            ]
                                        )
                                        (Pine.ListExpression
                                            [ Pine.LiteralExpression Pine.stringAsValue_Literal
                                            , funcRecordLessTag
                                            ]
                                        )
                                    )
                            )
                        )

                LocalEnvironment localEnvExpectedDecls ->
                    let
                        currentEnv : List String
                        currentEnv =
                            List.map Tuple.first compilation.environmentFunctions

                        currentEnvCoversExpected : Bool
                        currentEnvCoversExpected =
                            List.take (List.length localEnvExpectedDecls) currentEnv == localEnvExpectedDecls

                        buildEnvironmentRecursive :
                            List Pine.Expression
                            -> List String
                            -> Result String Pine.Expression
                        buildEnvironmentRecursive alreadyMapped remainingToBeMapped =
                            case remainingToBeMapped of
                                [] ->
                                    Ok (Pine.ListExpression alreadyMapped)

                                nextExpectedFunctionName :: remainingExpectedFunctions ->
                                    case currentEnvironmentFunctionEntryFromName nextExpectedFunctionName of
                                        Nothing ->
                                            Err
                                                ("Function '"
                                                    ++ functionName
                                                    ++ "' expects environment function '"
                                                    ++ nextExpectedFunctionName
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
                                if localEnvExpectedDecls == [] then
                                    Ok (Pine.ListExpression [])

                                else
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
                                            (if funcParamCount == 0 then
                                                Pine.ParseAndEvalExpression
                                                    (Pine.ListExpression
                                                        [ expectedEnvironment
                                                        , Pine.ListExpression []
                                                        ]
                                                    )
                                                    getFunctionExpression

                                             else
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
    if arguments == [] then
        function

    else
        adaptivePartialApplicationExpression
            { function = function
            , arguments = arguments
            , applicationFunctionSource =
                case emitReferenceExpression environmentFunctionPartialApplicationName emitStack of
                    Err _ ->
                        Nothing

                    Ok appFunc ->
                        Just appFunc
            }


emitWrapperForPartialApplication : Pine.Expression -> Int -> Pine.Expression -> Pine.Expression
emitWrapperForPartialApplication envFunctionsExpression parameterCount innerExpression =
    if parameterCount == 0 then
        emitWrapperForPartialApplicationZero
            { getFunctionInnerExpression =
                innerExpression
                    |> Pine.encodeExpressionAsValue
                    |> Pine.LiteralExpression
            , getEnvFunctionsExpression = envFunctionsExpression
            }

    else
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
    if config.arguments == [] then
        config.function

    else
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

        ReferenceExpression _ ->
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

        [ first, second, third ] ->
            accumulated
                + estimatePineValueSize first
                + estimatePineValueSize second
                + estimatePineValueSize third

        first :: second :: third :: remaining ->
            estimatePineListValueSizeHelper
                (accumulated
                    + estimatePineValueSize first
                    + estimatePineValueSize second
                    + estimatePineValueSize third
                )
                remaining
