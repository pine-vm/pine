module ElmInteractive exposing (..)

import BigInt
import Dict
import Elm.Parser
import Elm.Processing
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
import ElmInteractiveCoreModules
import ElmInteractiveKernelModules
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Parser
import Pine
import Result.Extra
import Set


type InteractiveSubmission
    = ExpressionSubmission Elm.Syntax.Expression.Expression
    | DeclarationSubmission Elm.Syntax.Declaration.Declaration


type InteractiveContext
    = DefaultContext
    | CustomModulesContext { includeCoreModules : Bool, modulesTexts : List String }


type alias SubmissionResponse =
    { displayText : String }


type ElmValue
    = ElmList (List ElmValue)
    | ElmChar Char
    | ElmInteger BigInt.BigInt
    | ElmString String
    | ElmTag String (List ElmValue)
    | ElmRecord (List ( String, ElmValue ))
    | ElmInternal String


type alias ProjectParsedElmFile =
    { projectedModuleName : List String
    , fileText : String
    , parsedModule : Elm.Syntax.File.File
    }


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
    | StringTagExpression String Expression
      -- TODO: Explore translate RecordAccess
    | RecordAccessExpression String Expression


type alias DecodeAndEvaluateExpressionStructure =
    { expression : Expression
    , environment : Expression
    }


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
    | RecordFieldDeconstruction String


type alias CompilationStack =
    { moduleAliases : Dict.Dict (List String) (List String)
    , availableModules : Dict.Dict (List String) ElmModuleInCompilation
    , availableDeclarations : Dict.Dict String InternalDeclaration
    , elmValuesToExposeToGlobal : Dict.Dict String (List String)
    }


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


type InternalDeclaration
    = CompiledDeclaration Pine.Value
    | DeconstructionDeclaration Expression


type alias ElmFunctionDeclarationStruct =
    { arguments : List Elm.Syntax.Pattern.Pattern
    , expression : Elm.Syntax.Expression.Expression
    }


type alias ElmModuleInCompilation =
    { declarations : Dict.Dict String Pine.Value
    , choiceTypes : Dict.Dict String ElmModuleChoiceType
    }


type alias ElmModuleChoiceType =
    { tagsNames : Set.Set String
    }


submissionInInteractive : InteractiveContext -> List String -> String -> Result String SubmissionResponse
submissionInInteractive context previousSubmissions submission =
    case compileEvalContextForElmInteractive context of
        Err error ->
            Err ("Failed to prepare the initial context: " ++ error)

        Ok initialContext ->
            submissionWithHistoryInInteractive initialContext previousSubmissions submission


submissionWithHistoryInInteractive : Pine.EvalContext -> List String -> String -> Result String SubmissionResponse
submissionWithHistoryInInteractive initialContext previousSubmissions submission =
    case previousSubmissions of
        [] ->
            submissionInInteractiveInPineContext initialContext submission
                |> Result.map Tuple.second

        firstSubmission :: remainingPreviousSubmissions ->
            case submissionInInteractiveInPineContext initialContext firstSubmission of
                Err _ ->
                    submissionWithHistoryInInteractive initialContext remainingPreviousSubmissions submission

                Ok ( expressionContext, _ ) ->
                    submissionWithHistoryInInteractive expressionContext remainingPreviousSubmissions submission


submissionInInteractiveInPineContext : Pine.EvalContext -> String -> Result String ( Pine.EvalContext, SubmissionResponse )
submissionInInteractiveInPineContext expressionContext submission =
    compileInteractiveSubmission expressionContext.environment submission
        |> Result.andThen
            (\pineExpression ->
                case Pine.evaluateExpression expressionContext pineExpression of
                    Err error ->
                        Err ("Failed to evaluate expression:\n" ++ Pine.displayStringFromPineError error)

                    Ok (Pine.BlobValue _) ->
                        Err "Type mismatch: Pine expression evaluated to a blob"

                    Ok (Pine.ListValue [ newState, responseValue ]) ->
                        submissionResponseFromResponsePineValue responseValue
                            |> Result.map (Tuple.pair { environment = newState })

                    Ok (Pine.ListValue resultList) ->
                        Err
                            ("Type mismatch: Pine expression evaluated to a list with unexpected number of elements: "
                                ++ String.fromInt (List.length resultList)
                                ++ " instead of 2"
                            )
            )


submissionResponseFromResponsePineValue : Pine.Value -> Result String SubmissionResponse
submissionResponseFromResponsePineValue responseValue =
    case pineValueAsElmValue responseValue of
        Err error ->
            Err ("Failed to encode as Elm value: " ++ error)

        Ok valueAsElmValue ->
            Ok { displayText = Tuple.first (elmValueAsExpression valueAsElmValue) }


elmValueAsExpression : ElmValue -> ( String, { needsParens : Bool } )
elmValueAsExpression elmValue =
    let
        applyNeedsParens : ( String, { needsParens : Bool } ) -> String
        applyNeedsParens ( expressionString, { needsParens } ) =
            if needsParens then
                "(" ++ expressionString ++ ")"

            else
                expressionString
    in
    case elmValue of
        ElmList list ->
            if Maybe.withDefault False (elmListItemsLookLikeTupleItems list) then
                ( "(" ++ (list |> List.map (elmValueAsExpression >> Tuple.first) |> String.join ",") ++ ")"
                , { needsParens = False }
                )

            else
                ( "[" ++ (list |> List.map (elmValueAsExpression >> Tuple.first) |> String.join ",") ++ "]"
                , { needsParens = False }
                )

        ElmInteger integer ->
            ( integer |> BigInt.toString
            , { needsParens = False }
            )

        ElmChar char ->
            ( "'" ++ (char |> String.fromChar) ++ "'"
            , { needsParens = False }
            )

        ElmString string ->
            ( string |> Json.Encode.string |> Json.Encode.encode 0
            , { needsParens = False }
            )

        ElmRecord fields ->
            ( if fields == [] then
                "{}"

              else
                "{ "
                    ++ (fields
                            |> List.map
                                (\( fieldName, fieldValue ) ->
                                    fieldName ++ " = " ++ Tuple.first (elmValueAsExpression fieldValue)
                                )
                            |> String.join ", "
                       )
                    ++ " }"
            , { needsParens = False }
            )

        ElmTag tagName tagArguments ->
            let
                defaultForTag () =
                    ( tagName
                        :: (tagArguments |> List.map (elmValueAsExpression >> applyNeedsParens))
                        |> String.join " "
                    , { needsParens = tagArguments /= [] }
                    )
            in
            if tagName == "Set_elm_builtin" then
                case tagArguments of
                    [ singleArgument ] ->
                        case List.map Tuple.first (elmValueDictToList singleArgument) of
                            [] ->
                                ( "Set.empty"
                                , { needsParens = False }
                                )

                            setElements ->
                                ( "Set.fromList ["
                                    ++ String.join ","
                                        (setElements
                                            |> List.map (elmValueAsExpression >> Tuple.first)
                                        )
                                    ++ "]"
                                , { needsParens = True }
                                )

                    _ ->
                        defaultForTag ()

            else if tagName == "RBEmpty_elm_builtin" then
                ( "Dict.empty"
                , { needsParens = False }
                )

            else
                case elmValueDictToList elmValue of
                    [] ->
                        defaultForTag ()

                    dictToList ->
                        ( "Dict.fromList ["
                            ++ String.join ","
                                (dictToList
                                    |> List.map
                                        (\( key, value ) ->
                                            "("
                                                ++ Tuple.first (elmValueAsExpression key)
                                                ++ ","
                                                ++ Tuple.first (elmValueAsExpression value)
                                                ++ ")"
                                        )
                                )
                            ++ "]"
                        , { needsParens = True }
                        )

        ElmInternal desc ->
            ( "<" ++ desc ++ ">"
            , { needsParens = False }
            )


elmValueAsJson : ElmValue -> Json.Encode.Value
elmValueAsJson elmValue =
    case elmValue of
        ElmInteger integer ->
            integer
                |> BigInt.toString
                |> Json.Encode.string

        ElmChar char ->
            Json.Encode.string (String.fromChar char)

        ElmString string ->
            Json.Encode.string string

        ElmList list ->
            Json.Encode.list elmValueAsJson list

        ElmRecord fields ->
            Json.Encode.list (\( fieldName, fieldValue ) -> Json.Encode.list identity [ Json.Encode.string fieldName, elmValueAsJson fieldValue ]) fields

        ElmTag tagName tagArguments ->
            Json.Encode.list identity [ Json.Encode.string tagName, Json.Encode.list elmValueAsJson tagArguments ]

        ElmInternal _ ->
            Json.Encode.string (Tuple.first (elmValueAsExpression elmValue))


pineValueAsElmValue : Pine.Value -> Result String ElmValue
pineValueAsElmValue pineValue =
    if pineValue == Pine.trueValue then
        Ok (ElmTag "True" [])

    else if pineValue == Pine.falseValue then
        Ok (ElmTag "False" [])

    else
        case pineValue of
            Pine.BlobValue blobValue ->
                case blobValue of
                    [] ->
                        Ok (ElmInternal "empty-blob")

                    firstByte :: _ ->
                        if firstByte == 4 || firstByte == 2 then
                            blobValue
                                |> Pine.bigIntFromBlobValue
                                |> Result.map ElmInteger

                        else if 10 < List.length blobValue then
                            case Pine.decodeExpressionFromValue pineValue of
                                Ok _ ->
                                    Ok (ElmInternal "expression")

                                Err _ ->
                                    Ok (ElmInternal "___error_skipped_large_blob___")

                        else
                            blobValue
                                |> Pine.bigIntFromUnsignedBlobValue
                                |> BigInt.toString
                                |> String.toInt
                                |> Maybe.withDefault 0
                                |> Char.fromCode
                                |> ElmChar
                                |> Ok

            Pine.ListValue list ->
                case list |> List.map pineValueAsElmValue |> Result.Extra.combine of
                    Err error ->
                        Err ("Failed to combine list: " ++ error)

                    Ok listValues ->
                        let
                            resultAsList =
                                Ok (ElmList listValues)
                        in
                        if listValues == [] then
                            resultAsList

                        else
                            case listValues of
                                [ ElmList tagNameChars, ElmList tagArguments ] ->
                                    case tryMapElmValueToString tagNameChars of
                                        Just tagName ->
                                            if stringStartsWithUpper tagName then
                                                if tagName == elmRecordTypeTagName then
                                                    (case tagArguments of
                                                        [ recordValue ] ->
                                                            elmValueAsElmRecord recordValue

                                                        _ ->
                                                            Err ("Wrong number of tag arguments: " ++ String.fromInt (List.length tagArguments))
                                                    )
                                                        |> Result.mapError ((++) "Failed to extract value under record tag: ")

                                                else if tagName == elmStringTypeTagName then
                                                    (case tagArguments of
                                                        [ ElmList charsList ] ->
                                                            charsList
                                                                |> tryMapElmValueToString
                                                                |> Maybe.map (ElmString >> Ok)
                                                                |> Maybe.withDefault (Err "Failed to map chars")

                                                        _ ->
                                                            Err "Unexpected shape of tag arguments"
                                                    )
                                                        |> Result.mapError ((++) "Failed to extract value under String tag: ")

                                                else
                                                    Ok (ElmTag tagName tagArguments)

                                            else
                                                resultAsList

                                        Nothing ->
                                            resultAsList

                                _ ->
                                    resultAsList


elmValueAsElmRecord : ElmValue -> Result String ElmValue
elmValueAsElmRecord elmValue =
    let
        tryMapToRecordField possiblyRecordField =
            case possiblyRecordField of
                ElmList fieldListItems ->
                    case fieldListItems of
                        [ fieldNameValue, fieldValue ] ->
                            let
                                continueWithFieldName fieldName =
                                    if not (stringStartsWithUpper fieldName) then
                                        Ok ( fieldName, fieldValue )

                                    else
                                        Err ("Field name does start with uppercase: '" ++ fieldName ++ "'")
                            in
                            case fieldNameValue of
                                ElmList fieldNameValueList ->
                                    case tryMapElmValueToString fieldNameValueList of
                                        Just fieldName ->
                                            continueWithFieldName fieldName

                                        Nothing ->
                                            Err "Failed parsing field name value."

                                ElmString fieldName ->
                                    continueWithFieldName fieldName

                                _ ->
                                    Err "Unexpected type in field name value."

                        _ ->
                            Err ("Unexpected number of list items: " ++ String.fromInt (List.length fieldListItems))

                _ ->
                    Err "Not a list."
    in
    case elmValue of
        ElmList recordFieldList ->
            case
                recordFieldList
                    |> List.map tryMapToRecordField
                    |> Result.Extra.combine
            of
                Ok recordFields ->
                    let
                        recordFieldsNames =
                            List.map Tuple.first recordFields
                    in
                    if List.sort recordFieldsNames == recordFieldsNames then
                        Ok (ElmRecord recordFields)

                    else
                        Err "Unexpected order of fields."

                Err parseFieldError ->
                    Err ("Failed to parse field: " ++ parseFieldError)

        _ ->
            Err "Value is not a list."


tryMapElmValueToChar : ElmValue -> Maybe Char
tryMapElmValueToChar elmValue =
    case elmValue of
        ElmChar char ->
            Just char

        _ ->
            Nothing


tryMapElmValueToString : List ElmValue -> Maybe String
tryMapElmValueToString elmValues =
    elmValues
        |> List.map tryMapElmValueToChar
        |> Maybe.Extra.combine
        |> Maybe.map String.fromList


elmValueDictToList : ElmValue -> List ( ElmValue, ElmValue )
elmValueDictToList =
    elmValueDictFoldr
        (\key value acc -> ( key, value ) :: acc)
        []


{-| Analog to <https://github.com/elm/core/blob/65cea00afa0de03d7dda0487d964a305fc3d58e3/src/Dict.elm#L547-L554>

    foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
    foldr func acc t =
        case t of
            RBEmpty_elm_builtin ->
                acc

            RBNode_elm_builtin _ key value left right ->
                foldr func (func key value (foldr func acc right)) left

-}
elmValueDictFoldr : (ElmValue -> ElmValue -> b -> b) -> b -> ElmValue -> b
elmValueDictFoldr func acc dict =
    case dict of
        ElmTag "RBEmpty_elm_builtin" _ ->
            acc

        ElmTag "RBNode_elm_builtin" [ _, key, value, left, right ] ->
            elmValueDictFoldr func (func key value (elmValueDictFoldr func acc right)) left

        _ ->
            acc


elmListItemsLookLikeTupleItems : List ElmValue -> Maybe Bool
elmListItemsLookLikeTupleItems list =
    if 3 < List.length list then
        Just False

    else
        list
            |> areAlmValueListItemTypesEqual
            |> Maybe.map not


areAlmValueListItemTypesEqual : List ElmValue -> Maybe Bool
areAlmValueListItemTypesEqual list =
    let
        pairsTypesEqual =
            list
                |> List.Extra.uniquePairs
                |> List.map (\( left, right ) -> areElmValueTypesEqual left right)
    in
    if List.all ((==) (Just True)) pairsTypesEqual then
        Just True

    else if List.any ((==) (Just False)) pairsTypesEqual then
        Just False

    else
        Nothing


areElmValueTypesEqual : ElmValue -> ElmValue -> Maybe Bool
areElmValueTypesEqual valueA valueB =
    case ( valueA, valueB ) of
        ( ElmInteger _, ElmInteger _ ) ->
            Just True

        ( ElmChar _, ElmChar _ ) ->
            Just True

        ( ElmString _, ElmString _ ) ->
            Just True

        ( ElmList _, ElmList _ ) ->
            Nothing

        ( ElmRecord recordA, ElmRecord recordB ) ->
            if Set.fromList (List.map Tuple.first recordA) /= Set.fromList (List.map Tuple.first recordB) then
                Just False

            else
                Nothing

        ( ElmTag _ _, ElmTag _ _ ) ->
            Nothing

        ( ElmInternal _, ElmInternal _ ) ->
            Nothing

        _ ->
            Just False


compileEvalContextForElmInteractive : InteractiveContext -> Result String Pine.EvalContext
compileEvalContextForElmInteractive context =
    let
        contextModulesTexts =
            case context of
                DefaultContext ->
                    ElmInteractiveCoreModules.elmCoreModulesTexts
                        ++ ElmInteractiveKernelModules.elmKernelModulesTexts

                CustomModulesContext { includeCoreModules, modulesTexts } ->
                    [ if includeCoreModules then
                        ElmInteractiveCoreModules.elmCoreModulesTexts
                            ++ ElmInteractiveKernelModules.elmKernelModulesTexts

                      else
                        []
                    , modulesTexts
                    ]
                        |> List.concat
    in
    expandElmInteractiveEnvironmentWithModuleTexts Pine.emptyEvalContext.environment contextModulesTexts
        |> Result.map (\result -> { environment = result.environment })


expandElmInteractiveEnvironmentWithModuleTexts :
    Pine.Value
    -> List String
    -> Result String { addedModulesNames : List (List String), environment : Pine.Value }
expandElmInteractiveEnvironmentWithModuleTexts environmentBefore contextModulesTexts =
    case getDeclarationsFromEnvironment environmentBefore of
        Err error ->
            Err ("Failed to get declarations from environment: " ++ error)

        Ok environmentBeforeDeclarations ->
            case separateEnvironmentDeclarations environmentBeforeDeclarations of
                Err err ->
                    Err ("Failed to separate declarations from environment: " ++ err)

                Ok separateEnvironmentDeclarationsBefore ->
                    contextModulesTexts
                        |> List.map parsedElmFileFromOnlyFileText
                        |> Result.Extra.combine
                        |> Result.andThen
                            (\parsedElmFiles ->
                                let
                                    modulesNamesWithDependencies =
                                        parsedElmFiles
                                            |> List.map
                                                (\file ->
                                                    file.parsedModule
                                                        |> listModuleTransitiveDependencies (List.map .parsedModule parsedElmFiles)
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
                            )
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
                                            |> List.map (Tuple.mapFirst (String.join "."))
                                            |> List.map (Tuple.mapSecond emitModuleValue)
                                in
                                { addedModulesNames = Dict.keys contextModules
                                , environment =
                                    Pine.environmentFromDeclarations
                                        (Dict.toList environmentBeforeDeclarations ++ modulesValues)
                                }
                            )


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


parsedElmFileFromOnlyFileText : String -> Result String ProjectParsedElmFile
parsedElmFileFromOnlyFileText fileText =
    case parseElmModuleText fileText of
        Err parseError ->
            [ [ "Failed to parse the module text with " ++ String.fromInt (List.length parseError) ++ " errors:" ]
            , parseError
                |> List.map parserDeadEndToString
            , [ "Module text was as follows:"
              , fileText
              ]
            ]
                |> List.concat
                |> String.join "\n"
                |> Err

        Ok parsedModule ->
            Ok
                { fileText = fileText
                , parsedModule = parsedModule
                , projectedModuleName = Elm.Syntax.Node.value (moduleNameFromSyntaxFile parsedModule)
                }


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
                declarationsFromChoiceTypesTags |> Dict.map (always CompiledDeclaration)
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
                        , List.filter (Tuple.first >> exposeFunction) functionDeclarations
                        , declarationsValuesForInfix
                        ]
                            |> List.concat
                            |> Dict.fromList
                  , choiceTypes = choiceTypes
                  }
                )


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
            Ok (LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt integer)))

        Elm.Syntax.Expression.Hex integer ->
            Ok (LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt integer)))

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
                |> Result.map LetBlockExpression

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
    -> Result String LetBlockStruct
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
                                (\expression ->
                                    { declarations = List.concat letEntries
                                    , expression = expression
                                    }
                                )
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


expressionForDeconstructions : List Deconstruction -> Expression -> Expression
expressionForDeconstructions =
    List.map expressionForDeconstruction
        >> List.foldr (>>) identity


expressionForDeconstruction : Deconstruction -> Expression -> Expression
expressionForDeconstruction deconstruction =
    case deconstruction of
        RecordFieldDeconstruction fieldName ->
            RecordAccessExpression fieldName

        ListItemDeconstruction index ->
            listItemFromIndexExpression index

        SkipItemsDeconstruction count ->
            listSkipExpression count


pineExpressionForDeconstructions : List Deconstruction -> Pine.Expression -> Pine.Expression
pineExpressionForDeconstructions =
    List.map pineExpressionForDeconstruction
        >> List.foldr (>>) identity


pineExpressionForDeconstruction : Deconstruction -> Pine.Expression -> Pine.Expression
pineExpressionForDeconstruction deconstruction =
    case deconstruction of
        RecordFieldDeconstruction fieldName ->
            pineExpressionForRecordAccess fieldName

        ListItemDeconstruction index ->
            listItemFromIndexExpression_Pine index

        SkipItemsDeconstruction count ->
            listSkipExpression_Pine count


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

        RecordAccessExpression _ recordExpression ->
            listDependenciesOfExpression dependenciesRelations recordExpression
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
                |> emitWrapperForPartialApplication [] argumentsCount
                |> evaluateAsIndependentExpression
                |> Result.withDefault
                    (Pine.valueFromString "Failed to compile choice type tag constructor")
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

                stack =
                    { stackBefore
                        | availableDeclarations =
                            stackBefore.availableDeclarations
                                |> Dict.union
                                    (Dict.map (always DeconstructionDeclaration)
                                        (Dict.fromList deconstructionDeclarations)
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
                                LetBlockExpression
                                    { declarations = deconstructionDeclarations
                                    , expression = expression
                                    }
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
                                        [ LiteralExpression
                                            (Pine.valueFromBigInt
                                                (BigInt.fromInt (List.length listItems))
                                            )
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
            continueWithOnlyEqualsCondition (LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt int)))

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
                        |> List.map (\fieldName -> ( fieldName, [ RecordFieldDeconstruction fieldName ] ))
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
        |> Result.map (ListExpression >> List.singleton >> tagValueExpression elmRecordTypeTagName)


compileElmSyntaxRecordAccess :
    CompilationStack
    -> String
    -> Elm.Syntax.Expression.Expression
    -> Result String Expression
compileElmSyntaxRecordAccess stack fieldName recordElmExpression =
    compileElmSyntaxExpression stack recordElmExpression
        |> Result.mapError ((++) "Failed to compile record expression: ")
        |> Result.map (RecordAccessExpression fieldName)


booleanConjunctionExpressionFromList : Expression -> List Expression -> Expression
booleanConjunctionExpressionFromList defaultIfEmpty operands =
    case operands of
        [] ->
            defaultIfEmpty

        firstOperator :: otherOperators ->
            otherOperators
                |> List.foldl
                    (\single aggregate -> applyKernelFunctionWithTwoArguments "logical_and" aggregate single)
                    firstOperator


listItemFromIndexExpression : Int -> Expression -> Expression
listItemFromIndexExpression itemIndex listExpression =
    pineKernel_ListHead (listSkipExpression itemIndex listExpression)


countListElementsExpression : Expression -> Expression
countListElementsExpression listExpression =
    KernelApplicationExpression
        { functionName = "length"
        , argument = listExpression
        }


countListElementsExpression_Pine : Pine.Expression -> Pine.Expression
countListElementsExpression_Pine listExpression =
    Pine.KernelApplicationExpression
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


compileElmFunctionOrValueLookupWithoutLocalResolution : ( List String, String ) -> CompilationStack -> Result String Expression
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


shouldInlineDeclaration : String -> Pine.Value -> Bool
shouldInlineDeclaration name value =
    if stringStartsWithUpper name then
        True

    else
        estimatePineValueSize value < 30 * 1000


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

        RecordAccessExpression fieldName recordExpr ->
            recordExpr
                |> emitExpression stack
                |> Result.map (pineExpressionForRecordAccess fieldName)


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
        |> emitExpressionInDeclarationBlock stackBefore
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
                |> closurizeFunctionExpressions
                    emitStack
                    (environmentDeclarationsAsFunctions |> List.map Tuple.first |> Set.fromList)
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

        RecordAccessExpression field record ->
            RecordAccessExpression field (mapReferencesForClosureCaptures closureCapturesByFunctionName record)


closurizeFunctionExpressions : EmitStack -> Set.Set String -> Expression -> Expression
closurizeFunctionExpressions stack closureCaptures expression =
    case expression of
        LiteralExpression _ ->
            expression

        ListExpression list ->
            ListExpression (List.map (closurizeFunctionExpressions stack closureCaptures) list)

        KernelApplicationExpression kernelApplication ->
            KernelApplicationExpression
                { kernelApplication
                    | argument =
                        closurizeFunctionExpressions stack closureCaptures kernelApplication.argument
                }

        ConditionalExpression conditional ->
            ConditionalExpression
                { condition =
                    closurizeFunctionExpressions stack closureCaptures conditional.condition
                , ifTrue =
                    closurizeFunctionExpressions stack closureCaptures conditional.ifTrue
                , ifFalse =
                    closurizeFunctionExpressions stack closureCaptures conditional.ifFalse
                }

        ReferenceExpression _ ->
            expression

        FunctionExpression functionParams functionBody ->
            let
                dependencies =
                    listDependenciesOfExpression stack functionBody

                closureCapturesForFunction =
                    Set.intersect closureCaptures dependencies
                        |> Set.toList

                closureFunctionParameters =
                    closureCapturesForFunction
                        |> List.map (Tuple.pair >> (|>) [] >> List.singleton)

                closureFunctionParameter =
                    closureParameterFromParameters closureFunctionParameters

                functionBodyMapped =
                    functionBody |> closurizeFunctionExpressions stack closureCaptures
            in
            if closureCapturesForFunction == [] then
                FunctionExpression functionParams functionBodyMapped

            else
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
                    List.map (closurizeFunctionExpressions stack closureCaptures) arguments

                mappedFunctionExpression =
                    closurizeFunctionExpressions stack closureCaptures functionExpression
            in
            FunctionApplicationExpression
                mappedFunctionExpression
                mappedArguments

        LetBlockExpression letBlock ->
            LetBlockExpression
                { letBlock
                    | declarations =
                        letBlock.declarations
                            |> List.map
                                (\( name, declExpression ) ->
                                    ( name
                                    , closurizeFunctionExpressions stack closureCaptures declExpression
                                    )
                                )
                    , expression =
                        closurizeFunctionExpressions stack closureCaptures letBlock.expression
                }

        StringTagExpression tag tagged ->
            StringTagExpression tag (closurizeFunctionExpressions stack closureCaptures tagged)

        RecordAccessExpression field record ->
            RecordAccessExpression field (closurizeFunctionExpressions stack closureCaptures record)


environmentDeconstructionsFromFunctionParams : List FunctionParam -> Dict.Dict String EnvironmentDeconstructionEntry
environmentDeconstructionsFromFunctionParams =
    closureParameterFromParameters
        >> Dict.fromList


closureParameterFromParameters : List FunctionParam -> FunctionParam
closureParameterFromParameters =
    List.indexedMap
        (\paramIndex ->
            List.map (Tuple.mapSecond ((::) (ListItemDeconstruction paramIndex)))
        )
        >> List.concat


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

        RecordAccessExpression fieldName record ->
            let
                ( recordDeclarations, recordExpression ) =
                    liftDeclsFromLetBlocksRecursively record
            in
            ( recordDeclarations
            , RecordAccessExpression fieldName recordExpression
            )


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

        RecordAccessExpression field record ->
            RecordAccessExpression
                field
                (mapLocalDeclarationNamesInDescendants localSet mapDeclarationName record)


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


inlineApplicationsOfEnvironmentDeclarations : EmitStack -> List ( String, Expression ) -> Expression -> Expression
inlineApplicationsOfEnvironmentDeclarations stackBeforeAddingDeps environmentDeclarations =
    let
        newReferencesDependencies =
            environmentDeclarations
                |> List.map (Tuple.mapSecond (listDependenciesOfExpression stackBeforeAddingDeps))
                |> Dict.fromList

        stackWithEnvironmentDeclDeps =
            { stackBeforeAddingDeps
                | declarationsDependencies = Dict.union newReferencesDependencies stackBeforeAddingDeps.declarationsDependencies
            }

        environmentDeclarationsDict =
            Dict.fromList environmentDeclarations

        findReplacement expr =
            case expr of
                FunctionApplicationExpression (ReferenceExpression functionName) arguments ->
                    case Dict.get functionName environmentDeclarationsDict of
                        Nothing ->
                            Nothing

                        Just appliedFunction ->
                            let
                                ( appliedFunctionParams, appliedFunctionBody ) =
                                    parseFunctionParameters appliedFunction

                                dependencies =
                                    listDependenciesOfExpression stackWithEnvironmentDeclDeps appliedFunction
                            in
                            if
                                (List.length arguments /= List.length appliedFunctionParams)
                                    || Set.member functionName dependencies
                            then
                                Nothing

                            else
                                let
                                    replacementsDict : Dict.Dict String Expression
                                    replacementsDict =
                                        appliedFunctionParams
                                            |> List.indexedMap
                                                (\paramIndex paramDeconstructions ->
                                                    arguments
                                                        |> List.drop paramIndex
                                                        |> List.head
                                                        |> Maybe.map
                                                            (\argumentExpr ->
                                                                paramDeconstructions
                                                                    |> List.map
                                                                        (Tuple.mapSecond
                                                                            (expressionForDeconstructions
                                                                                >> (|>) argumentExpr
                                                                            )
                                                                        )
                                                            )
                                                        |> Maybe.withDefault []
                                                )
                                            |> List.concat
                                            |> Dict.fromList

                                    findReplacementForReference innerExpr =
                                        case innerExpr of
                                            ReferenceExpression innerReference ->
                                                Dict.get innerReference replacementsDict

                                            _ ->
                                                Nothing
                                in
                                appliedFunctionBody
                                    |> transformExpressionWithOptionalReplacement findReplacementForReference
                                    |> inlineApplicationsOfEnvironmentDeclarations stackWithEnvironmentDeclDeps environmentDeclarations
                                    |> Just

                _ ->
                    Nothing
    in
    transformExpressionWithOptionalReplacement findReplacement


transformExpressionWithOptionalReplacement : (Expression -> Maybe Expression) -> Expression -> Expression
transformExpressionWithOptionalReplacement findReplacement expression =
    case findReplacement expression of
        Just replacement ->
            replacement

        Nothing ->
            case expression of
                LiteralExpression _ ->
                    expression

                ListExpression list ->
                    ListExpression (List.map (transformExpressionWithOptionalReplacement findReplacement) list)

                KernelApplicationExpression kernelApplication ->
                    KernelApplicationExpression
                        { kernelApplication
                            | argument =
                                transformExpressionWithOptionalReplacement findReplacement kernelApplication.argument
                        }

                ConditionalExpression conditional ->
                    ConditionalExpression
                        { condition =
                            transformExpressionWithOptionalReplacement findReplacement conditional.condition
                        , ifTrue =
                            transformExpressionWithOptionalReplacement findReplacement conditional.ifTrue
                        , ifFalse =
                            transformExpressionWithOptionalReplacement findReplacement conditional.ifFalse
                        }

                ReferenceExpression _ ->
                    expression

                FunctionExpression functionParam functionBody ->
                    FunctionExpression
                        functionParam
                        (transformExpressionWithOptionalReplacement findReplacement functionBody)

                FunctionApplicationExpression functionExpression arguments ->
                    let
                        mappedArguments =
                            List.map (transformExpressionWithOptionalReplacement findReplacement) arguments

                        mappedFunctionExpression =
                            transformExpressionWithOptionalReplacement findReplacement functionExpression
                    in
                    FunctionApplicationExpression
                        mappedFunctionExpression
                        mappedArguments

                LetBlockExpression letBlock ->
                    LetBlockExpression
                        { letBlock
                            | declarations =
                                letBlock.declarations
                                    |> List.map (Tuple.mapSecond (transformExpressionWithOptionalReplacement findReplacement))
                            , expression =
                                transformExpressionWithOptionalReplacement findReplacement letBlock.expression
                        }

                StringTagExpression tag tagged ->
                    StringTagExpression tag (transformExpressionWithOptionalReplacement findReplacement tagged)

                RecordAccessExpression field record ->
                    RecordAccessExpression field (transformExpressionWithOptionalReplacement findReplacement record)


getDeclarationValueFromCompilation : ( List String, String ) -> CompilationStack -> Result String Pine.Value
getDeclarationValueFromCompilation ( localModuleName, nameInModule ) compilation =
    let
        canonicalModuleName =
            Dict.get localModuleName compilation.moduleAliases
                |> Maybe.withDefault localModuleName
    in
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


parseFunctionParameters : Expression -> ( List FunctionParam, Expression )
parseFunctionParameters expression =
    case expression of
        FunctionExpression functionParams functionBody ->
            let
                ( innerParams, innerBody ) =
                    parseFunctionParameters functionBody
            in
            ( functionParams ++ innerParams, innerBody )

        _ ->
            ( [], expression )


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


moduleNameFromSyntaxFile : Elm.Syntax.File.File -> Elm.Syntax.Node.Node (List String)
moduleNameFromSyntaxFile file =
    case Elm.Syntax.Node.value file.moduleDefinition of
        Elm.Syntax.Module.NormalModule normalModule ->
            normalModule.moduleName

        Elm.Syntax.Module.PortModule portModule ->
            portModule.moduleName

        Elm.Syntax.Module.EffectModule effectModule ->
            effectModule.moduleName


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


valueFromString : String -> Pine.Value
valueFromString =
    Pine.valueFromString >> List.singleton >> tagValue elmStringTypeTagName


tagValue : String -> List Pine.Value -> Pine.Value
tagValue tagName tagArguments =
    Pine.ListValue [ Pine.valueFromString tagName, Pine.ListValue tagArguments ]


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
    , ( "<<", 9 )
    , ( ">>", 9 )
    ]
        |> Dict.fromList


{-| The expression evaluates to a list with two elements:
The first element contains the new interactive session state for the possible next submission.
The second element contains the response, the value to display to the user.
-}
compileInteractiveSubmission : Pine.Value -> String -> Result String Pine.Expression
compileInteractiveSubmission environment submission =
    case
        getDeclarationsFromEnvironment environment |> Result.andThen separateEnvironmentDeclarations
    of
        Err error ->
            Err ("Failed to get declarations from environment: " ++ error)

        Ok environmentDeclarations ->
            let
                buildExpressionForNewStateAndResponse config =
                    Pine.ListExpression
                        [ config.newStateExpression
                        , config.responseExpression
                        ]

                ( defaultCompilationStack, emitStack ) =
                    compilationAndEmitStackFromInteractiveEnvironment environmentDeclarations
            in
            case parseInteractiveSubmissionFromString submission of
                Err error ->
                    Ok
                        (buildExpressionForNewStateAndResponse
                            { newStateExpression = Pine.EnvironmentExpression
                            , responseExpression =
                                Pine.LiteralExpression (Pine.valueFromString ("Failed to parse submission: " ++ error))
                            }
                        )

                Ok (DeclarationSubmission elmDeclaration) ->
                    case elmDeclaration of
                        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                            let
                                declarationName =
                                    Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name

                                compilationStack =
                                    { defaultCompilationStack
                                        | availableDeclarations =
                                            defaultCompilationStack.availableDeclarations
                                                |> Dict.remove declarationName
                                    }
                            in
                            case
                                compileElmSyntaxFunction compilationStack functionDeclaration
                                    |> Result.map Tuple.second
                                    |> Result.andThen
                                        (\functionDeclarationCompilation ->
                                            emitExpressionInDeclarationBlock
                                                emitStack
                                                (Dict.singleton declarationName functionDeclarationCompilation)
                                                functionDeclarationCompilation
                                        )
                                    |> Result.andThen evaluateAsIndependentExpression
                            of
                                Err error ->
                                    Err ("Failed to compile Elm function declaration: " ++ error)

                                Ok declarationValue ->
                                    Ok
                                        (buildExpressionForNewStateAndResponse
                                            { newStateExpression =
                                                Pine.KernelApplicationExpression
                                                    { functionName = "concat"
                                                    , argument =
                                                        Pine.ListExpression
                                                            [ Pine.ListExpression
                                                                [ Pine.LiteralExpression
                                                                    (Pine.valueFromContextExpansionWithName
                                                                        ( declarationName
                                                                        , declarationValue
                                                                        )
                                                                    )
                                                                ]
                                                            , Pine.EnvironmentExpression
                                                            ]
                                                    }
                                            , responseExpression =
                                                Pine.LiteralExpression (Pine.valueFromString ("Declared " ++ declarationName))
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

                Ok (ExpressionSubmission elmExpression) ->
                    case
                        compileElmSyntaxExpression defaultCompilationStack elmExpression
                            |> Result.andThen (emitExpressionInDeclarationBlock emitStack Dict.empty)
                    of
                        Err error ->
                            Err ("Failed to compile Elm to Pine expression: " ++ error)

                        Ok pineExpression ->
                            Ok
                                (buildExpressionForNewStateAndResponse
                                    { newStateExpression = Pine.EnvironmentExpression
                                    , responseExpression = pineExpression
                                    }
                                )


compilationAndEmitStackFromInteractiveEnvironment :
    { modules : Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    , otherDeclarations : Dict.Dict String Pine.Value
    }
    -> ( CompilationStack, EmitStack )
compilationAndEmitStackFromInteractiveEnvironment environmentDeclarations =
    let
        defaultCompilationStack =
            { moduleAliases = Dict.empty
            , availableModules = environmentDeclarations.modules
            , availableDeclarations =
                environmentDeclarations.otherDeclarations |> Dict.map (always CompiledDeclaration)
            , elmValuesToExposeToGlobal = elmValuesToExposeToGlobalDefault
            }

        implicitImportStatements =
            environmentDeclarations.modules
                |> Dict.keys
                |> List.map
                    (\moduleName ->
                        { canonicalModuleName = moduleName
                        , localModuleName = moduleName
                        , exposingList = Nothing
                        }
                    )

        moduleImports =
            moduleImportsFromCompilationStack implicitImportStatements defaultCompilationStack

        importedDeclarations =
            moduleImports.importedDeclarations
                |> Dict.union environmentDeclarations.otherDeclarations

        compilationStack =
            { moduleAliases = Dict.empty
            , availableModules = environmentDeclarations.modules
            , availableDeclarations =
                environmentDeclarations.otherDeclarations |> Dict.map (always CompiledDeclaration)
            , elmValuesToExposeToGlobal = elmValuesToExposeToGlobalDefault
            }

        emitStack =
            { moduleImports =
                { moduleImports
                    | importedDeclarations = importedDeclarations
                }
            , declarationsDependencies = Dict.empty
            , environmentFunctions = []
            , environmentDeconstructions = Dict.empty
            }
    in
    ( compilationStack, emitStack )


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


parseInteractiveSubmissionFromString : String -> Result String InteractiveSubmission
parseInteractiveSubmissionFromString submission =
    let
        unified =
            String.replace "\n" " " submission
    in
    if
        String.contains " = " unified
            && not (String.startsWith "let " (String.trim unified))
            && not (String.startsWith "{" (String.trim submission))
    then
        parseDeclarationFromString submission
            |> Result.mapError parserDeadEndsToString
            |> Result.Extra.join
            |> Result.map DeclarationSubmission

    else
        parseExpressionFromString submission
            |> Result.mapError parserDeadEndsToString
            |> Result.Extra.join
            |> Result.map ExpressionSubmission


parseExpressionFromString : String -> Result (List Parser.DeadEnd) (Result String Elm.Syntax.Expression.Expression)
parseExpressionFromString expressionCode =
    -- https://github.com/stil4m/elm-syntax/issues/34
    let
        indentAmount =
            4

        indentedExpressionCode =
            expressionCode
                |> String.lines
                |> List.map ((++) (String.repeat indentAmount (String.fromChar ' ')))
                |> String.join "\n"

        declarationTextBeforeExpression =
            "wrapping_expression_in_function = \n"
    in
    parseDeclarationFromString (declarationTextBeforeExpression ++ indentedExpressionCode)
        |> Result.mapError (List.map (mapLocationForPrefixText declarationTextBeforeExpression >> mapLocationForIndentAmount indentAmount))
        |> Result.map
            (Result.andThen
                (\declaration ->
                    case declaration of
                        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                            functionDeclaration
                                |> .declaration
                                |> Elm.Syntax.Node.value
                                |> .expression
                                |> Elm.Syntax.Node.value
                                |> Ok

                        _ ->
                            Err "Failed to extract the wrapping function."
                )
            )


parseDeclarationFromString : String -> Result (List Parser.DeadEnd) (Result String Elm.Syntax.Declaration.Declaration)
parseDeclarationFromString declarationCode =
    -- https://github.com/stil4m/elm-syntax/issues/34
    let
        moduleTextBeforeDeclaration =
            """
module Main exposing (..)


"""

        moduleText =
            moduleTextBeforeDeclaration ++ declarationCode
    in
    parseElmModuleText moduleText
        |> Result.mapError (List.map (mapLocationForPrefixText moduleTextBeforeDeclaration))
        |> Result.map
            (.declarations
                >> List.map Elm.Syntax.Node.value
                >> List.head
                >> Result.fromMaybe "Failed to extract the declaration from the parsed module."
            )


mapLocationForPrefixText : String -> Parser.DeadEnd -> Parser.DeadEnd
mapLocationForPrefixText prefixText =
    let
        prefixLines =
            String.lines prefixText
    in
    mapLocation
        { row = 1 - List.length prefixLines
        , col = -(prefixLines |> List.reverse |> List.head |> Maybe.withDefault "" |> String.length)
        }


mapLocationForIndentAmount : Int -> Parser.DeadEnd -> Parser.DeadEnd
mapLocationForIndentAmount indentAmount =
    mapLocation { row = 0, col = -indentAmount }


mapLocation : { row : Int, col : Int } -> Parser.DeadEnd -> Parser.DeadEnd
mapLocation offset deadEnd =
    { deadEnd | row = deadEnd.row + offset.row, col = deadEnd.col + offset.col }


parseElmModuleTextToJson : String -> String
parseElmModuleTextToJson elmModule =
    let
        jsonValue =
            case parseElmModuleText elmModule of
                Err _ ->
                    [ ( "Err", "Failed to parse this as module text" |> Json.Encode.string ) ] |> Json.Encode.object

                Ok file ->
                    [ ( "Ok", file |> Elm.Syntax.File.encode ) ] |> Json.Encode.object
    in
    jsonValue |> Json.Encode.encode 0


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    Elm.Parser.parse >> Result.map (Elm.Processing.process Elm.Processing.init)


parserDeadEndsToString : List Parser.DeadEnd -> String
parserDeadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map parserDeadEndToString deadEnds))


parserDeadEndToString : Parser.DeadEnd -> String
parserDeadEndToString deadend =
    parserProblemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


parserProblemToString : Parser.Problem -> String
parserProblemToString p =
    case p of
        Parser.Expecting s ->
            "expecting '" ++ s ++ "'"

        Parser.ExpectingInt ->
            "expecting int"

        Parser.ExpectingHex ->
            "expecting hex"

        Parser.ExpectingOctal ->
            "expecting octal"

        Parser.ExpectingBinary ->
            "expecting binary"

        Parser.ExpectingFloat ->
            "expecting float"

        Parser.ExpectingNumber ->
            "expecting number"

        Parser.ExpectingVariable ->
            "expecting variable"

        Parser.ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        Parser.ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        Parser.ExpectingEnd ->
            "expecting end"

        Parser.UnexpectedChar ->
            "unexpected char"

        Parser.Problem s ->
            "problem " ++ s

        Parser.BadRepeat ->
            "bad repeat"


stringStartsWithUpper : String -> Bool
stringStartsWithUpper =
    String.uncons >> Maybe.map (Tuple.first >> Char.isUpper) >> Maybe.withDefault False


json_encode_pineValue : Dict.Dict String Pine.Value -> Pine.Value -> Json.Encode.Value
json_encode_pineValue dictionary value =
    let
        blobDict =
            dictionary
                |> Dict.toList
                |> List.filterMap
                    (\( entryName, entryValue ) ->
                        case entryValue of
                            Pine.BlobValue blob ->
                                Just ( blob, entryName )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList

        listDict =
            dictionary
                |> Dict.toList
                |> List.filterMap
                    (\( entryName, entryValue ) ->
                        case entryValue of
                            Pine.ListValue list ->
                                Just ( list, entryName )

                            _ ->
                                Nothing
                    )
                |> List.foldl
                    (\( nextList, nextName ) intermediateDict ->
                        let
                            hash =
                                pineListValueFastHash nextList

                            assocList =
                                intermediateDict
                                    |> Dict.get hash
                                    |> Maybe.withDefault []
                                    |> (::) ( nextList, nextName )
                        in
                        intermediateDict
                            |> Dict.insert hash assocList
                    )
                    Dict.empty
    in
    json_encode_pineValue_Internal
        { blobDict = blobDict, listDict = listDict }
        value


json_encode_pineValue_Internal :
    { blobDict : Dict.Dict (List Int) String
    , listDict : Dict.Dict Int (List ( List Pine.Value, String ))
    }
    -> Pine.Value
    -> Json.Encode.Value
json_encode_pineValue_Internal dictionary value =
    case value of
        Pine.ListValue list ->
            let
                defaultListEncoding () =
                    Json.Encode.object
                        [ ( "List", Json.Encode.list (json_encode_pineValue_Internal dictionary) list ) ]
            in
            if list == [] then
                defaultListEncoding ()

            else
                case
                    dictionary.listDict
                        |> Dict.get (pineListValueFastHash list)
                        |> Maybe.andThen (List.Extra.find (Tuple.first >> (==) list))
                        |> Maybe.map Tuple.second
                of
                    Just reference ->
                        Json.Encode.object
                            [ ( "Reference", Json.Encode.string reference ) ]

                    Nothing ->
                        case Pine.stringFromListValue list of
                            Err _ ->
                                defaultListEncoding ()

                            Ok asString ->
                                Json.Encode.object
                                    [ ( "ListAsString", Json.Encode.string asString ) ]

        Pine.BlobValue blob ->
            case dictionary.blobDict |> Dict.get blob of
                Just reference ->
                    Json.Encode.object
                        [ ( "Reference", Json.Encode.string reference ) ]

                Nothing ->
                    Json.Encode.object
                        [ ( "Blob", Json.Encode.list Json.Encode.int blob ) ]


json_decode_pineValue : Json.Decode.Decoder ( Pine.Value, Dict.Dict String Pine.Value )
json_decode_pineValue =
    json_decode_pineValueWithDictionary Dict.empty


json_decode_pineValueWithDictionary :
    Dict.Dict String Pine.Value
    -> Json.Decode.Decoder ( Pine.Value, Dict.Dict String Pine.Value )
json_decode_pineValueWithDictionary parentDictionary =
    json_decode_optionalNullableField "Dictionary" json_decode_pineValueDictionary
        |> Json.Decode.andThen
            (Maybe.map
                (Dict.union (Dict.map (always LiteralValue) parentDictionary)
                    >> resolveDictionaryToLiteralValues
                    >> Result.Extra.unpack Json.Decode.fail Json.Decode.succeed
                )
                >> Maybe.withDefault (Json.Decode.succeed parentDictionary)
            )
        |> Json.Decode.andThen
            (\mergedDictionary ->
                json_decode_pineValueApplyingDictionary mergedDictionary
                    |> Json.Decode.map (Tuple.pair >> (|>) mergedDictionary)
            )


json_decode_pineValueDictionary : Json.Decode.Decoder (Dict.Dict String PineValueSupportingReference)
json_decode_pineValueDictionary =
    Json.Decode.list json_decode_pineValueDictionaryEntry
        |> Json.Decode.map Dict.fromList


resolveDictionaryToLiteralValues : Dict.Dict String PineValueSupportingReference -> Result String (Dict.Dict String Pine.Value)
resolveDictionaryToLiteralValues dictionary =
    dictionary
        |> Dict.toList
        |> List.map
            (\( entryName, entryValue ) ->
                resolvePineValueReferenceToLiteralRecursive Set.empty dictionary entryValue
                    |> Result.map (Tuple.pair entryName)
                    |> Result.mapError
                        (\( errorStack, errorMessage ) ->
                            "Failed to resolve entry '"
                                ++ entryName
                                ++ "': "
                                ++ errorMessage
                                ++ " ("
                                ++ String.join ", " errorStack
                                ++ ")"
                        )
            )
        |> Result.Extra.combine
        |> Result.map Dict.fromList


resolvePineValueReferenceToLiteralRecursive :
    Set.Set String
    -> Dict.Dict String PineValueSupportingReference
    -> PineValueSupportingReference
    -> Result ( List String, String ) Pine.Value
resolvePineValueReferenceToLiteralRecursive stack dictionary valueSupportingRef =
    case valueSupportingRef of
        LiteralValue literal ->
            Ok literal

        ListSupportingReference list ->
            list
                |> List.map (resolvePineValueReferenceToLiteralRecursive stack dictionary)
                |> Result.Extra.combine
                |> Result.map Pine.ListValue

        ReferenceValue reference ->
            if Set.member reference stack then
                Err ( [], "cyclic reference" )

            else
                case Dict.get reference dictionary of
                    Nothing ->
                        let
                            keys =
                                Dict.keys dictionary
                        in
                        Err
                            ( []
                            , "Did not find dictionary entry for reference '"
                                ++ reference
                                ++ "'. Dictionary contains "
                                ++ String.fromInt (Dict.size dictionary)
                                ++ " entries between "
                                ++ Maybe.withDefault "" (List.head keys)
                                ++ " and "
                                ++ Maybe.withDefault "" (List.head (List.reverse keys))
                            )

                    Just foundEntry ->
                        resolvePineValueReferenceToLiteralRecursive
                            (Set.insert reference stack)
                            dictionary
                            foundEntry
                            |> Result.mapError (Tuple.mapFirst ((::) reference))


json_decode_pineValueDictionaryEntry : Json.Decode.Decoder ( String, PineValueSupportingReference )
json_decode_pineValueDictionaryEntry =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "value" json_decode_pineValueSupportingReference)


json_decode_pineValueApplyingDictionary : Dict.Dict String Pine.Value -> Json.Decode.Decoder Pine.Value
json_decode_pineValueApplyingDictionary dictionary =
    json_decode_pineValueGeneric
        { decodeListElement =
            Json.Decode.lazy (\_ -> json_decode_pineValueWithDictionary dictionary |> Json.Decode.map Tuple.first)
        , consList = Pine.ListValue
        , decodeReference =
            \reference ->
                case Dict.get reference dictionary of
                    Nothing ->
                        Json.Decode.fail ("Did not find declaration for reference '" ++ reference ++ "'")

                    Just resolvedValue ->
                        Json.Decode.succeed resolvedValue
        , consLiteral = identity
        }


json_decode_pineValueSupportingReference : Json.Decode.Decoder PineValueSupportingReference
json_decode_pineValueSupportingReference =
    json_decode_pineValueGeneric
        { decodeListElement = Json.Decode.lazy (\_ -> json_decode_pineValueSupportingReference)
        , consList = ListSupportingReference
        , decodeReference = ReferenceValue >> Json.Decode.succeed
        , consLiteral = LiteralValue
        }


type PineValueSupportingReference
    = ListSupportingReference (List PineValueSupportingReference)
    | LiteralValue Pine.Value
    | ReferenceValue String


type alias DecodePineValueConfig value listElement =
    { decodeListElement : Json.Decode.Decoder listElement
    , consList : List listElement -> value
    , decodeReference : String -> Json.Decode.Decoder value
    , consLiteral : Pine.Value -> value
    }


json_decode_pineValueGeneric : DecodePineValueConfig value listElement -> Json.Decode.Decoder value
json_decode_pineValueGeneric config =
    Json.Decode.oneOf
        [ Json.Decode.field "List"
            (Json.Decode.list config.decodeListElement |> Json.Decode.map config.consList)
        , Json.Decode.field "ListAsString" Json.Decode.string
            |> Json.Decode.map (Pine.valueFromString >> config.consLiteral)
        , Json.Decode.field "Blob" (Json.Decode.list Json.Decode.int)
            |> Json.Decode.map (Pine.BlobValue >> config.consLiteral)
        , Json.Decode.field "Reference"
            (Json.Decode.string
                |> Json.Decode.andThen config.decodeReference
            )
        ]


pineListValueFastHash : List Pine.Value -> Int
pineListValueFastHash list =
    list
        |> List.indexedMap
            (\index entry ->
                (case entry of
                    Pine.BlobValue blob ->
                        71 * List.length blob

                    Pine.ListValue innerList ->
                        7919 * List.length innerList
                )
                    * (index + 1)
            )
        |> List.sum
        |> (+) (List.length list)


json_decode_optionalNullableField : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
json_decode_optionalNullableField fieldName decoder =
    Json.Decode.map (Maybe.andThen identity)
        (json_decode_optionalField fieldName (Json.Decode.nullable decoder))


json_decode_optionalField : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
json_decode_optionalField fieldName decoder =
    let
        finishDecoding json =
            case Json.Decode.decodeValue (Json.Decode.field fieldName Json.Decode.value) json of
                Ok _ ->
                    -- The field is present, so run the decoder on it.
                    Json.Decode.map Just (Json.Decode.field fieldName decoder)

                Err _ ->
                    -- The field was missing, which is fine!
                    Json.Decode.succeed Nothing
    in
    Json.Decode.value
        |> Json.Decode.andThen finishDecoding


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


expressionAsJson : Expression -> Json.Encode.Value
expressionAsJson expression =
    (case expression of
        LiteralExpression literal ->
            [ ( "Literal"
              , case Pine.stringFromValue literal of
                    Err _ ->
                        Json.Encode.object []

                    Ok asString ->
                        Json.Encode.string asString
              )
            ]

        ListExpression list ->
            [ ( "List"
              , list |> Json.Encode.list expressionAsJson
              )
            ]

        KernelApplicationExpression kernelApplication ->
            [ ( "KernelApplication"
              , Json.Encode.object
                    [ ( "argument", expressionAsJson kernelApplication.argument )
                    , ( "functionName", Json.Encode.string kernelApplication.functionName )
                    ]
              )
            ]

        ConditionalExpression conditional ->
            [ ( "Conditional"
              , [ ( "condition", .condition )
                , ( "ifFalse", .ifFalse )
                , ( "ifTrue", .ifTrue )
                ]
                    |> List.map (Tuple.mapSecond ((|>) conditional >> expressionAsJson))
                    |> Json.Encode.object
              )
            ]

        ReferenceExpression name ->
            [ ( "Reference"
              , [ ( "name", Json.Encode.string name )
                ]
                    |> Json.Encode.object
              )
            ]

        FunctionExpression functionParam functionBody ->
            [ ( "Function"
              , [ ( "parameters"
                  , functionParam
                        |> Json.Encode.list (Json.Encode.list (Tuple.first >> Json.Encode.string))
                  )
                , ( "body"
                  , functionBody |> expressionAsJson
                  )
                ]
                    |> Json.Encode.object
              )
            ]

        FunctionApplicationExpression functionExpression arguments ->
            [ ( "FunctionApplication"
              , [ ( "function"
                  , functionExpression
                        |> expressionAsJson
                  )
                , ( "arguments"
                  , arguments
                        |> Json.Encode.list expressionAsJson
                  )
                ]
                    |> Json.Encode.object
              )
            ]

        LetBlockExpression _ ->
            [ ( "LetBlock"
              , []
                    |> Json.Encode.object
              )
            ]

        StringTagExpression tag expr ->
            [ ( "StringTag"
              , Json.Encode.object
                    [ ( "tag", Json.Encode.string tag )
                    , ( "expr", expressionAsJson expr )
                    ]
              )
            ]

        RecordAccessExpression _ _ ->
            [ ( "RecordAccess"
              , Json.Encode.object []
              )
            ]
    )
        |> Json.Encode.object
