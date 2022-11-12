module ElmInteractive exposing (..)

import BigInt
import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.Type
import ElmInteractiveCoreModules
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
    | InitContextFromApp { modulesTexts : List String }


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
    | IndependentFromEnvironmentExpression Pine.Expression
    | ListExpression (List Expression)
    | DecodeAndEvaluateExpression DecodeAndEvaluateExpressionStructure
    | KernelApplicationExpression KernelApplicationExpressionStructure
    | ConditionalExpression ConditionalExpressionStructure
    | ReferenceExpression String
    | FunctionExpression FunctionExpressionStruct
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


type alias FunctionExpressionStruct =
    { argumentDeconstructions : List ( String, Pine.Expression -> Pine.Expression )
    , expression : Expression
    }


type EnvironmentElement
    = LiteralEnvironmentElement Expression
    | DeconstructionEnvironmentElement (Pine.Expression -> Pine.Expression)
    | ForwardedEnvironmentElement


type alias CompilationStack =
    { moduleAliases : Dict.Dict (List String) (List String)
    , availableModules : Dict.Dict (List String) ElmModuleInCompilation
    , availableDeclarations : Dict.Dict String InternalDeclaration
    , inliningParentDeclarations : Set.Set String
    , elmValuesToExposeToGlobal : Dict.Dict String (List String)
    }


type alias EmitStack =
    { declarationsDependencies : Dict.Dict String (Set.Set String)
    , appEnvElementsOrder : List String
    }


type InternalDeclaration
    = CompiledDeclaration Pine.Expression
    | ElmFunctionDeclaration ElmFunctionDeclarationStruct
    | DeconstructionDeclaration Expression


type alias ElmFunctionDeclarationStruct =
    { arguments : List Elm.Syntax.Pattern.Pattern
    , expression : Elm.Syntax.Expression.Expression
    }


type alias ElmModuleInCompilation =
    Dict.Dict String Pine.Expression


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
            Ok { displayText = elmValueAsExpression valueAsElmValue }


elmValueAsExpression : ElmValue -> String
elmValueAsExpression elmValue =
    case elmValue of
        ElmList list ->
            "[" ++ (list |> List.map elmValueAsExpression |> String.join ",") ++ "]"

        ElmInteger integer ->
            integer |> BigInt.toString

        ElmChar char ->
            "'" ++ (char |> String.fromChar) ++ "'"

        ElmString string ->
            string |> Json.Encode.string |> Json.Encode.encode 0

        ElmRecord fields ->
            if fields == [] then
                "{}"

            else
                "{ " ++ (fields |> List.map (\( fieldName, fieldValue ) -> fieldName ++ " = " ++ elmValueAsExpression fieldValue) |> String.join ", ") ++ " }"

        ElmTag tagName tagArguments ->
            tagName :: (tagArguments |> List.map elmValueAsExpression) |> String.join " "

        ElmInternal desc ->
            "<" ++ desc ++ ">"


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
            Json.Encode.string (elmValueAsExpression elmValue)


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
                            tryMapToChar elmValue =
                                case elmValue of
                                    ElmChar char ->
                                        Just char

                                    _ ->
                                        Nothing

                            resultAsList =
                                Ok (ElmList listValues)
                        in
                        if listValues == [] then
                            resultAsList

                        else
                            case listValues of
                                [ ElmString tagName, ElmList tagArguments ] ->
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
                                                [ ElmString string ] ->
                                                    Ok (ElmString string)

                                                [ ElmList charsList ] ->
                                                    case charsList |> List.map tryMapToChar |> Maybe.Extra.combine of
                                                        Just chars ->
                                                            chars |> String.fromList |> ElmString |> Ok

                                                        Nothing ->
                                                            Err "Failed to map chars"

                                                _ ->
                                                    Err "Unexpected shape of tag arguments"
                                            )
                                                |> Result.mapError ((++) "Failed to extract value under String tag: ")

                                        else
                                            Ok (ElmTag tagName tagArguments)

                                    else
                                        resultAsList

                                _ ->
                                    case listValues |> List.map tryMapToChar |> Maybe.Extra.combine of
                                        Just chars ->
                                            chars |> String.fromList |> ElmString |> Ok

                                        Nothing ->
                                            resultAsList


elmValueAsElmRecord : ElmValue -> Result String ElmValue
elmValueAsElmRecord elmValue =
    let
        tryMapToRecordField possiblyRecordField =
            case possiblyRecordField of
                ElmList [ ElmString fieldName, fieldValue ] ->
                    if not (stringStartsWithUpper fieldName) then
                        Ok ( fieldName, fieldValue )

                    else
                        Err ("Field name does start with uppercase: '" ++ fieldName ++ "'")

                _ ->
                    Err "Not a list."
    in
    case elmValue of
        ElmList recordFieldList ->
            case recordFieldList |> List.map tryMapToRecordField |> Result.Extra.combine of
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


compileEvalContextForElmInteractive : InteractiveContext -> Result String Pine.EvalContext
compileEvalContextForElmInteractive context =
    let
        contextModulesTexts =
            case context of
                DefaultContext ->
                    ElmInteractiveCoreModules.elmCoreModulesTexts

                InitContextFromApp { modulesTexts } ->
                    ElmInteractiveCoreModules.elmCoreModulesTexts ++ modulesTexts
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
                                                ++ String.join "." (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value file.parsedModule.moduleDefinition))
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
                                                                    (Dict.fromList moduleValue)
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


listModuleTransitiveDependencies : List Elm.Syntax.File.File -> Elm.Syntax.File.File -> Result String (List (List String))
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
            if List.member (Elm.Syntax.Node.value (moduleNameFromSyntaxFile file)) moduleNamesWithoutImplicitImport then
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
        Err _ ->
            Err ("Failed to parse the module text: " ++ fileText)

        Ok parsedModule ->
            Ok
                { fileText = fileText
                , parsedModule = parsedModule
                , projectedModuleName = Elm.Syntax.Node.value (moduleNameFromSyntaxFile parsedModule)
                }


compileElmModuleTextIntoNamedExports :
    Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    -> ProjectParsedElmFile
    -> Result String ( Elm.Syntax.ModuleName.ModuleName, List ( String, Pine.Expression ) )
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

        declarationsFromCustomTypes : Dict.Dict String Pine.Expression
        declarationsFromCustomTypes =
            moduleToTranslate.parsedModule.declarations
                |> List.map Elm.Syntax.Node.value
                |> List.concatMap
                    (\declaration ->
                        case declaration of
                            Elm.Syntax.Declaration.CustomTypeDeclaration customTypeDeclaration ->
                                customTypeDeclaration.constructors
                                    |> List.map
                                        (Elm.Syntax.Node.value
                                            >> compileElmSyntaxValueConstructor
                                        )

                            _ ->
                                []
                    )
                |> Dict.fromList
                |> Dict.map
                    (\name originalDeclaredValue ->
                        elmDeclarationsOverrides
                            |> Dict.get moduleName
                            |> Maybe.andThen (Dict.get name)
                            |> Maybe.withDefault originalDeclaredValue
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

        initialCompilationStack =
            { moduleAliases = moduleAliases
            , availableModules = availableModules
            , availableDeclarations =
                (localFunctionDeclarations |> Dict.map (always internalDeclarationFromFunction))
                    |> Dict.union (declarationsFromCustomTypes |> Dict.map (always CompiledDeclaration))
            , inliningParentDeclarations = Set.empty
            , elmValuesToExposeToGlobal =
                elmValuesToExposeToGlobalDefault
                    |> Dict.filter (always ((==) moduleName >> not))
            }

        initialEmitStack =
            { declarationsDependencies = Dict.empty
            , appEnvElementsOrder = []
            }

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

        localFunctionsResult =
            localFunctionDeclarations
                |> Dict.toList
                |> List.map
                    (\( functionName, functionDeclaration ) ->
                        compileElmSyntaxFunction initialCompilationStack functionDeclaration
                            |> Result.mapError ((++) ("Failed to compile function '" ++ functionName ++ "': "))
                    )
                |> Result.Extra.combine
                |> Result.andThen (emitClosureExpressions initialEmitStack)
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
                , Dict.toList declarationsFromCustomTypes
                    ++ functionDeclarations
                    ++ declarationsValuesForInfix
                )


internalDeclarationFromFunction : Elm.Syntax.Expression.Function -> InternalDeclaration
internalDeclarationFromFunction elmFunction =
    ElmFunctionDeclaration
        { arguments = (Elm.Syntax.Node.value elmFunction.declaration).arguments |> List.map Elm.Syntax.Node.value
        , expression = Elm.Syntax.Node.value (Elm.Syntax.Node.value elmFunction.declaration).expression
        }


moduleNamesWithoutImplicitImport : List (List String)
moduleNamesWithoutImplicitImport =
    autoImportedModulesNames
        ++ [ [ "Char" ]
           , [ "Tuple" ]
           ]


autoImportedModulesNames : List (List String)
autoImportedModulesNames =
    [ [ "Basics" ]
    , [ "Maybe" ]
    , [ "List" ]
    , [ "String" ]
    ]


elmValuesToExposeToGlobalDefault : Dict.Dict String (List String)
elmValuesToExposeToGlobalDefault =
    [ ( "identity", [ "Basics" ] )
    , ( "always", [ "Basics" ] )
    , ( "not", [ "Basics" ] )
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
    , ( "True", [ "Basics" ] )
    , ( "False", [ "Basics" ] )
    , ( "(::)", [ "List" ] )
    , ( "Nothing", [ "Maybe" ] )
    , ( "Just", [ "Maybe" ] )
    ]
        |> Dict.fromList


elmDeclarationsOverrides : Dict.Dict (List String) (Dict.Dict String Pine.Expression)
elmDeclarationsOverrides =
    [ ( [ "Basics" ]
      , [ ( "True"
          , Pine.trueValue
                |> Pine.LiteralExpression
          )
        , ( "False"
          , Pine.falseValue
                |> Pine.LiteralExpression
          )
        ]
            |> Dict.fromList
      )
    ]
        |> Dict.fromList


addInliningParentDeclaration : String -> CompilationStack -> CompilationStack
addInliningParentDeclaration name compilation =
    { compilation
        | inliningParentDeclarations = compilation.inliningParentDeclarations |> Set.insert name
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
                            { functionName = "neg_int"
                            , argument = negatedExpression
                            }
                        )

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            if moduleName == [] then
                compileElmFunctionOrValueLookup localName stack

            else
                getDeclarationValueFromCompilation ( moduleName, localName ) stack
                    |> Result.map IndependentFromEnvironmentExpression

        Elm.Syntax.Expression.Application application ->
            case application |> List.map Elm.Syntax.Node.value of
                [] ->
                    Err "Invalid shape of application: Zero elements in the list"

                appliedFunctionElmSyntax :: elmArguments ->
                    case elmArguments |> List.map (compileElmSyntaxExpression stack) |> Result.Extra.combine of
                        Err error ->
                            Err ("Failed to compile Elm arguments: " ++ error)

                        Ok arguments ->
                            let
                                continueWithNonKernelApplication =
                                    case compileElmSyntaxExpression stack appliedFunctionElmSyntax of
                                        Err error ->
                                            Err ("Failed to compile Elm function syntax: " ++ error)

                                        Ok appliedFunctionSyntax ->
                                            Ok
                                                (positionalApplicationExpressionFromListOfArguments
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
                                        continueWithNonKernelApplication

                                _ ->
                                    continueWithNonKernelApplication

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
                                        compileElmFunctionOrValueLookup ("(" ++ operator ++ ")") stack
                                            |> Result.map
                                                (\operationFunction ->
                                                    DecodeAndEvaluateExpression
                                                        { expression =
                                                            DecodeAndEvaluateExpression
                                                                { expression = operationFunction
                                                                , environment = leftExpression
                                                                }
                                                        , environment = rightExpression
                                                        }
                                                )
                                    )
                        )
                    |> Result.mapError ((++) ("Failed to compile OperatorApplication '" ++ operator ++ "': "))

        Elm.Syntax.Expression.PrefixOperator operator ->
            compileElmFunctionOrValueLookup ("(" ++ operator ++ ")") stack

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

        _ ->
            Err
                ("Unsupported type of expression: "
                    ++ (elmExpression |> Elm.Syntax.Expression.encode |> Json.Encode.encode 0)
                )


compileElmSyntaxLetBlock :
    CompilationStack
    -> Elm.Syntax.Expression.LetBlock
    -> Result String LetBlockStruct
compileElmSyntaxLetBlock stackBefore letBlock =
    letBlock.declarations
        |> List.concatMap
            (\letDeclaration ->
                case Elm.Syntax.Node.value letDeclaration of
                    Elm.Syntax.Expression.LetFunction letFunction ->
                        [ Ok
                            ( Elm.Syntax.Node.value (Elm.Syntax.Node.value letFunction.declaration).name
                            , internalDeclarationFromFunction letFunction
                            )
                        ]

                    Elm.Syntax.Expression.LetDestructuring (Elm.Syntax.Node.Node _ pattern) (Elm.Syntax.Node.Node _ destructuredExpressionElm) ->
                        destructuredExpressionElm
                            |> compileElmSyntaxExpression stackBefore
                            |> Result.andThen
                                (\destructuredExpression ->
                                    pattern
                                        |> declarationsFromPattern
                                        |> Result.map
                                            (\declarations ->
                                                declarations
                                                    |> List.map
                                                        (\( declName, deconsExpr ) ->
                                                            ( declName
                                                            , DeconstructionDeclaration (deconsExpr destructuredExpression)
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
                        declarationsFromPattern (Elm.Syntax.Node.value patternNode)
                            |> Result.mapError (\error -> "Failed destructuring in let block: " ++ error)
                            |> Result.map (List.map (Tuple.mapSecond ((|>) compiledExpression)))
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
            |> List.map declarationsFromPattern_Pine
            |> Result.Extra.combine
    of
        Err error ->
            Err ("Failed to compile function argument pattern: " ++ error)

        Ok argumentsDeconstructDeclarationsBuilders ->
            function.expression
                |> compileElmSyntaxExpression stackBefore
                |> Result.map
                    (\expression ->
                        argumentsDeconstructDeclarationsBuilders
                            |> List.foldr
                                (\nextFunctionDeconstructions prevExpression ->
                                    FunctionExpression
                                        { argumentDeconstructions = nextFunctionDeconstructions
                                        , expression = prevExpression
                                        }
                                )
                                expression
                    )


declarationsFromPattern :
    Elm.Syntax.Pattern.Pattern
    -> Result String (List ( String, Expression -> Expression ))
declarationsFromPattern pattern =
    case pattern of
        Elm.Syntax.Pattern.VarPattern varName ->
            Ok [ ( varName, \deconstructedExpression -> deconstructedExpression ) ]

        Elm.Syntax.Pattern.AllPattern ->
            Ok []

        Elm.Syntax.Pattern.TuplePattern tupleElements ->
            let
                getTupleElementExpression =
                    listItemFromIndexExpression
            in
            case
                tupleElements
                    |> List.map Elm.Syntax.Node.value
                    |> List.map declarationsFromPattern
                    |> Result.Extra.combine
            of
                Err error ->
                    Err ("Failed to parse patterns from tuple element: " ++ error)

                Ok tupleElementsDeconstructions ->
                    tupleElementsDeconstructions
                        |> List.indexedMap
                            (\tupleElementIndex tupleElement ->
                                tupleElement
                                    |> List.map
                                        (Tuple.mapSecond
                                            (\deconstruct -> getTupleElementExpression tupleElementIndex >> deconstruct)
                                        )
                            )
                        |> List.concat
                        |> Ok

        Elm.Syntax.Pattern.RecordPattern fieldsElements ->
            fieldsElements
                |> List.map Elm.Syntax.Node.value
                |> List.map (\fieldName -> ( fieldName, RecordAccessExpression fieldName ))
                |> Ok

        _ ->
            Err ("Unsupported type of pattern: " ++ (pattern |> Elm.Syntax.Pattern.encode |> Json.Encode.encode 0))


declarationsFromPattern_Pine :
    Elm.Syntax.Pattern.Pattern
    -> Result String (List ( String, Pine.Expression -> Pine.Expression ))
declarationsFromPattern_Pine pattern =
    case pattern of
        Elm.Syntax.Pattern.VarPattern varName ->
            Ok [ ( varName, \deconstructedExpression -> deconstructedExpression ) ]

        Elm.Syntax.Pattern.AllPattern ->
            Ok []

        Elm.Syntax.Pattern.TuplePattern tupleElements ->
            let
                getTupleElementExpression =
                    listItemFromIndexExpression_Pine
            in
            case
                tupleElements
                    |> List.map Elm.Syntax.Node.value
                    |> List.map declarationsFromPattern_Pine
                    |> Result.Extra.combine
            of
                Err error ->
                    Err ("Failed to parse patterns from tuple element: " ++ error)

                Ok tupleElementsDeconstructions ->
                    tupleElementsDeconstructions
                        |> List.indexedMap
                            (\tupleElementIndex tupleElement ->
                                tupleElement
                                    |> List.map
                                        (Tuple.mapSecond
                                            (\deconstruct -> getTupleElementExpression tupleElementIndex >> deconstruct)
                                        )
                            )
                        |> List.concat
                        |> Ok

        Elm.Syntax.Pattern.RecordPattern fieldsElements ->
            fieldsElements
                |> List.map Elm.Syntax.Node.value
                |> List.map (\fieldName -> ( fieldName, pineExpressionForRecordAccess fieldName ))
                |> Ok

        _ ->
            Err ("Unsupported type of pattern: " ++ (pattern |> Elm.Syntax.Pattern.encode |> Json.Encode.encode 0))


listDependenciesOfExpression : Dict.Dict String (Set.Set String) -> Expression -> Set.Set String
listDependenciesOfExpression dependenciesRelations expression =
    (case expression of
        LiteralExpression _ ->
            Set.empty

        IndependentFromEnvironmentExpression _ ->
            Set.empty

        ListExpression list ->
            list
                |> List.map (listDependenciesOfExpression dependenciesRelations)
                |> List.foldl Set.union Set.empty

        DecodeAndEvaluateExpression decodeAndEvaluate ->
            [ decodeAndEvaluate.expression, decodeAndEvaluate.environment ]
                |> listDependenciesOfExpressions dependenciesRelations

        KernelApplicationExpression application ->
            listDependenciesOfExpression dependenciesRelations application.argument

        ConditionalExpression conditional ->
            [ conditional.condition, conditional.ifTrue, conditional.ifFalse ]
                |> listDependenciesOfExpressions dependenciesRelations

        ReferenceExpression reference ->
            Set.singleton reference

        FunctionExpression function ->
            let
                expressionDependencies =
                    listDependenciesOfExpression dependenciesRelations function.expression
            in
            function.argumentDeconstructions
                |> List.map Tuple.first
                |> List.foldl Set.remove expressionDependencies

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
        |> getTransitiveDependenciesStep dependenciesRelations


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


listDependenciesOfExpressions : Dict.Dict String (Set.Set String) -> List Expression -> Set.Set String
listDependenciesOfExpressions dependenciesRelations =
    List.map (listDependenciesOfExpression dependenciesRelations) >> List.foldl Set.union Set.empty


compileElmSyntaxValueConstructor : Elm.Syntax.Type.ValueConstructor -> ( String, Pine.Expression )
compileElmSyntaxValueConstructor valueConstructor =
    let
        constructorName =
            Elm.Syntax.Node.value valueConstructor.name
    in
    ( constructorName
    , case List.length valueConstructor.arguments of
        0 ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString constructorName)
                , Pine.ListExpression []
                ]

        1 ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString constructorName)
                , Pine.ListExpression [ Pine.EnvironmentExpression ]
                ]
                |> Pine.encodeExpressionAsValue
                |> Pine.LiteralExpression

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
                |> Pine.LiteralExpression

        argumentsCount ->
            Pine.LiteralExpression
                (Pine.valueFromString ("Compilation not implemented for this number of arguments: " ++ String.fromInt argumentsCount))
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
                            ConditionalExpression
                                { condition = deconstructedCase.conditionExpression
                                , ifTrue = deconstructedCase.thenExpression
                                , ifFalse = nextBlockExpression
                                }
                    in
                    Ok
                        (List.foldr
                            conditionalFromCase
                            (LiteralExpression (Pine.valueFromString "Error in case-of block: No matching branch."))
                            cases
                        )


compileElmSyntaxCaseBlockCase :
    CompilationStack
    -> Expression
    -> Elm.Syntax.Expression.Case
    ->
        Result
            String
            { conditionExpression : Expression
            , thenExpression : Expression
            }
compileElmSyntaxCaseBlockCase stackBefore caseBlockValueExpression ( elmPattern, elmExpression ) =
    case compileElmSyntaxPattern stackBefore caseBlockValueExpression elmPattern of
        Err error ->
            Err error

        Ok deconstruction ->
            let
                stack =
                    { stackBefore
                        | availableDeclarations =
                            stackBefore.availableDeclarations
                                |> Dict.union
                                    (Dict.map (always DeconstructionDeclaration)
                                        (Dict.fromList deconstruction.declarations)
                                    )
                    }
            in
            elmExpression
                |> Elm.Syntax.Node.value
                |> compileElmSyntaxExpression stack
                |> Result.map
                    (\expression ->
                        { conditionExpression = deconstruction.conditionExpression
                        , thenExpression =
                            LetBlockExpression
                                { declarations = deconstruction.declarations
                                , expression = expression
                                }
                        }
                    )


compileElmSyntaxPattern :
    CompilationStack
    -> Expression
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Result String { conditionExpression : Expression, declarations : List ( String, Expression ) }
compileElmSyntaxPattern stack deconstructedExpression elmPattern =
    let
        continueWithOnlyEqualsCondition valueToCompare =
            Ok
                { conditionExpression = equalCondition [ deconstructedExpression, valueToCompare ]
                , declarations = []
                }
    in
    case Elm.Syntax.Node.value elmPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok
                { conditionExpression = LiteralExpression Pine.trueValue
                , declarations = []
                }

        Elm.Syntax.Pattern.ListPattern listElements ->
            let
                conditionsAndDeclarationsFromPattern elementIndex =
                    compileElmSyntaxPattern stack
                        (listItemFromIndexExpression elementIndex deconstructedExpression)
                        >> Result.map
                            (\listElementResult ->
                                { conditions = [ listElementResult.conditionExpression ]
                                , declarations = listElementResult.declarations
                                }
                            )
            in
            listElements
                |> List.indexedMap conditionsAndDeclarationsFromPattern
                |> Result.Extra.combine
                |> Result.map
                    (\elementsResults ->
                        let
                            matchesLengthCondition =
                                equalCondition
                                    [ LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt (List.length listElements)))
                                    , countListElementsExpression deconstructedExpression
                                    ]

                            condition =
                                (matchesLengthCondition
                                    :: List.concatMap .conditions elementsResults
                                )
                                    |> booleanConjunctionExpressionFromList
                                        (equalCondition
                                            [ deconstructedExpression, ListExpression [] ]
                                        )

                            declarations =
                                elementsResults |> List.concatMap .declarations
                        in
                        { conditionExpression = condition
                        , declarations = declarations
                        }
                    )

        Elm.Syntax.Pattern.UnConsPattern unconsLeft unconsRight ->
            case ( Elm.Syntax.Node.value unconsLeft, Elm.Syntax.Node.value unconsRight ) of
                ( Elm.Syntax.Pattern.VarPattern unconsLeftName, Elm.Syntax.Pattern.VarPattern unconsRightName ) ->
                    let
                        declarations =
                            [ ( unconsLeftName
                              , pineKernel_ListHead deconstructedExpression
                              )
                            , ( unconsRightName
                              , listSkipExpression 1 deconstructedExpression
                              )
                            ]

                        conditionExpression =
                            KernelApplicationExpression
                                { functionName = "logical_not"
                                , argument =
                                    equalCondition
                                        [ deconstructedExpression
                                        , listSkipExpression 1 deconstructedExpression
                                        ]
                                }
                    in
                    Ok
                        { conditionExpression = conditionExpression
                        , declarations = declarations
                        }

                _ ->
                    Err "Unsupported shape of uncons pattern."

        Elm.Syntax.Pattern.NamedPattern qualifiedName customTypeArgumentPatterns ->
            let
                mapArgumentsToOnlyNameResults =
                    customTypeArgumentPatterns
                        |> List.map Elm.Syntax.Node.value
                        |> List.map
                            (\argumentPattern ->
                                case argumentPattern of
                                    Elm.Syntax.Pattern.VarPattern argumentName ->
                                        Ok argumentName

                                    Elm.Syntax.Pattern.AllPattern ->
                                        Ok "unused_from_elm_all_pattern"

                                    _ ->
                                        Err ("Unsupported type of pattern: " ++ (argumentPattern |> Elm.Syntax.Pattern.encode |> Json.Encode.encode 0))
                            )

                conditionExpression =
                    equalCondition
                        [ LiteralExpression (Pine.valueFromString qualifiedName.name)
                        , pineKernel_ListHead deconstructedExpression
                        ]
            in
            case mapArgumentsToOnlyNameResults |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to compile pattern in case block: " ++ error)

                Ok declarationsNames ->
                    let
                        declarations =
                            declarationsNames
                                |> List.indexedMap
                                    (\argumentIndex declarationName ->
                                        ( declarationName
                                        , listItemFromIndexExpression argumentIndex
                                            (listItemFromIndexExpression 1 deconstructedExpression)
                                        )
                                    )
                    in
                    Ok
                        { conditionExpression = conditionExpression
                        , declarations = declarations
                        }

        Elm.Syntax.Pattern.CharPattern char ->
            continueWithOnlyEqualsCondition (LiteralExpression (Pine.valueFromChar char))

        Elm.Syntax.Pattern.IntPattern int ->
            continueWithOnlyEqualsCondition (LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt int)))

        Elm.Syntax.Pattern.VarPattern name ->
            Ok
                { conditionExpression = LiteralExpression Pine.trueValue
                , declarations =
                    [ ( name
                      , deconstructedExpression
                      )
                    ]
                }

        _ ->
            Err
                ("Unsupported type of pattern in case-of block case: "
                    ++ Json.Encode.encode 0 (Elm.Syntax.Pattern.encode (Elm.Syntax.Node.value elmPattern))
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


pineKernel_ListHead : Expression -> Expression
pineKernel_ListHead listExpression =
    KernelApplicationExpression
        { functionName = "list_head"
        , argument = listExpression
        }


positionalApplicationExpressionFromListOfArguments : Expression -> List Expression -> Expression
positionalApplicationExpressionFromListOfArguments function arguments =
    case arguments of
        [] ->
            function

        nextArgument :: followingArguments ->
            positionalApplicationExpressionFromListOfArguments
                (DecodeAndEvaluateExpression
                    { expression = function
                    , environment = nextArgument
                    }
                )
                followingArguments


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


compileElmFunctionOrValueLookup : String -> CompilationStack -> Result String Expression
compileElmFunctionOrValueLookup name compilation =
    let
        continueWithoutLocalResolution _ =
            compileElmFunctionOrValueLookupWithoutLocalResolution name compilation
    in
    case compilation.availableDeclarations |> Dict.get name of
        Nothing ->
            continueWithoutLocalResolution ()

        Just (ElmFunctionDeclaration elmFunctionDeclaration) ->
            if compilation.inliningParentDeclarations |> Set.member name then
                continueWithoutLocalResolution ()

            else
                compileElmSyntaxFunctionWithoutName
                    (addInliningParentDeclaration name compilation)
                    elmFunctionDeclaration

        Just (CompiledDeclaration compiledDeclaration) ->
            Ok (IndependentFromEnvironmentExpression compiledDeclaration)

        Just (DeconstructionDeclaration deconstruction) ->
            Ok deconstruction


compileElmFunctionOrValueLookupWithoutLocalResolution : String -> CompilationStack -> Result String Expression
compileElmFunctionOrValueLookupWithoutLocalResolution name compilation =
    case Dict.get name compilation.elmValuesToExposeToGlobal of
        Nothing ->
            if stringStartsWithUpper name then
                case Dict.get name compilation.availableDeclarations of
                    Nothing ->
                        Err ("Missing declaration for '" ++ name ++ "'")

                    Just (CompiledDeclaration compiledDeclaration) ->
                        Ok (IndependentFromEnvironmentExpression compiledDeclaration)

                    Just (ElmFunctionDeclaration _) ->
                        Err ("Unexpected value for '" ++ name ++ "': Elm function declaration")

                    Just (DeconstructionDeclaration deconstruction) ->
                        Ok deconstruction

            else
                Ok (ReferenceExpression name)

        Just moduleName ->
            getDeclarationValueFromCompilation ( moduleName, name ) compilation
                |> Result.map IndependentFromEnvironmentExpression


emitExpression : EmitStack -> Expression -> Result String Pine.Expression
emitExpression stack expression =
    case expression of
        LiteralExpression literal ->
            Ok (Pine.LiteralExpression literal)

        IndependentFromEnvironmentExpression pineExpression ->
            Ok pineExpression

        ListExpression list ->
            list
                |> List.map (emitExpression stack)
                |> Result.Extra.combine
                |> Result.map Pine.ListExpression

        DecodeAndEvaluateExpression decodeAndEvaluate ->
            decodeAndEvaluate.expression
                |> emitExpression stack
                |> Result.andThen
                    (\function ->
                        decodeAndEvaluate.environment
                            |> emitExpression stack
                            |> Result.map
                                (\environment ->
                                    Pine.DecodeAndEvaluateExpression
                                        { expression = function
                                        , environment = environment
                                        }
                                )
                    )

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

        FunctionExpression function ->
            emitFunctionBindingArgumentToName stack function

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


emitLetBlock : EmitStack -> LetBlockStruct -> Result String Pine.Expression
emitLetBlock stackBefore letBlock =
    emitClosureExpression
        stackBefore
        letBlock.declarations
        letBlock.expression


{-| Builds an expression that captures parts of the current application argument into a literal and wraps that in a function application expression that will bind the next application argument to the given name, together with the earlier captured context.
In other words, it captures dependencies from the current environment and combines them with the function to enable transport to and reuse in other places.
-}
emitFunctionBindingArgumentToName : EmitStack -> FunctionExpressionStruct -> Result String Pine.Expression
emitFunctionBindingArgumentToName stackBefore function =
    let
        innerDependencies =
            listDependenciesOfExpression stackBefore.declarationsDependencies function.expression

        outerDependencies =
            function.argumentDeconstructions
                |> List.map Tuple.first
                |> List.foldl Set.remove innerDependencies

        environmentElements =
            [ List.map (Tuple.mapSecond DeconstructionEnvironmentElement) function.argumentDeconstructions
            , outerDependencies |> Set.toList |> List.map (Tuple.pair >> (|>) ForwardedEnvironmentElement)
            ]
                |> List.concat
    in
    emitApplicationArgumentExpression
        stackBefore
        environmentElements
        |> Result.andThen
            (\( stack, argumentExpression ) ->
                emitExpression stack function.expression
                    |> Result.map
                        (\functionExpression ->
                            Pine.ListExpression
                                [ Pine.LiteralExpression (Pine.valueFromString "DecodeAndEvaluate")
                                , Pine.ListExpression
                                    [ Pine.ListExpression
                                        [ Pine.LiteralExpression (Pine.valueFromString "expression")
                                        , functionExpression
                                            |> Pine.encodeExpressionAsValue
                                            |> Pine.LiteralExpression
                                            |> Pine.encodeExpressionAsValue
                                            |> Pine.LiteralExpression
                                        ]
                                    , Pine.ListExpression
                                        [ Pine.LiteralExpression (Pine.valueFromString "environment")
                                        , argumentExpression
                                        ]
                                    ]
                                ]
                        )
            )


emitClosureExpressions :
    EmitStack
    -> List ( String, Expression )
    -> Result String (List ( String, Pine.Expression ))
emitClosureExpressions stackBefore newDeclarations =
    emitClosureExpression stackBefore newDeclarations
        |> (\builder ->
                newDeclarations
                    |> List.map
                        (\( declarationName, declarationExpression ) ->
                            builder declarationExpression
                                |> Result.mapError ((++) ("Failed for declaration '" ++ declarationName ++ "': "))
                                |> Result.map (Tuple.pair declarationName)
                        )
                    |> Result.Extra.combine
           )


{-| Covers a block with declarations that might contain recursive functions, like a let-in block or an entire module.
-}
emitClosureExpression :
    EmitStack
    -> List ( String, Expression )
    -> Expression
    -> Result String Pine.Expression
emitClosureExpression stackBeforeAddingDependencies environmentDeclarations expressionInClosure =
    let
        newReferencesDependencies =
            environmentDeclarations
                |> List.map (Tuple.mapSecond (listDependenciesOfExpression stackBeforeAddingDependencies.declarationsDependencies))
                |> Dict.fromList

        stackBefore =
            { stackBeforeAddingDependencies
                | declarationsDependencies = Dict.union newReferencesDependencies stackBeforeAddingDependencies.declarationsDependencies
            }

        innerDependencies =
            listDependenciesOfExpression stackBefore.declarationsDependencies expressionInClosure

        outerDependencies =
            environmentDeclarations
                |> List.map Tuple.first
                |> List.foldl Set.remove innerDependencies

        usedDeclarations =
            environmentDeclarations
                |> List.filter (Tuple.first >> Set.member >> (|>) innerDependencies)

        environmentElements =
            [ List.map (Tuple.mapSecond LiteralEnvironmentElement) usedDeclarations
            , outerDependencies |> Set.toList |> List.map (Tuple.pair >> (|>) ForwardedEnvironmentElement)
            ]
                |> List.concat
    in
    if usedDeclarations == [] then
        emitExpression stackBefore expressionInClosure

    else
        emitApplicationArgumentExpression
            stackBefore
            environmentElements
            |> Result.andThen
                (\( stack, argumentExpression ) ->
                    emitExpression stack expressionInClosure
                        |> Result.map
                            (\expressionPine ->
                                Pine.DecodeAndEvaluateExpression
                                    { expression =
                                        expressionPine
                                            |> Pine.encodeExpressionAsValue
                                            |> Pine.LiteralExpression
                                    , environment =
                                        Pine.DecodeAndEvaluateExpression
                                            { expression = argumentExpression
                                            , environment = Pine.EnvironmentExpression
                                            }
                                    }
                            )
                )


emitApplicationArgumentExpression :
    EmitStack
    -> List ( String, EnvironmentElement )
    -> Result String ( EmitStack, Pine.Expression )
emitApplicationArgumentExpression stackBefore environmentElements =
    let
        stack =
            { stackBefore
                | appEnvElementsOrder = List.map Tuple.first environmentElements
            }

        buildEnvironmentElementExpression ( elementName, envExpansion ) =
            case envExpansion of
                LiteralEnvironmentElement literal ->
                    emitExpression stack literal
                        |> Result.mapError ((++) ("Failed emitting environment element '" ++ elementName ++ "': "))
                        |> Result.map
                            (\literalPine ->
                                [ Pine.LiteralExpression (Pine.valueFromString elementName)
                                , Pine.LiteralExpression (Pine.encodeExpressionAsValue literalPine)
                                ]
                                    |> Pine.ListExpression
                                    |> Pine.encodeExpressionAsValue
                                    |> Pine.LiteralExpression
                            )

                DeconstructionEnvironmentElement deconstructExpression ->
                    [ Pine.LiteralExpression (Pine.valueFromString elementName)
                    , Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "Literal")
                        , deconstructExpression Pine.EnvironmentExpression
                        ]
                    ]
                        |> Pine.ListExpression
                        |> Pine.encodeExpressionAsValue
                        |> Pine.LiteralExpression
                        |> Ok

                ForwardedEnvironmentElement ->
                    [ Pine.LiteralExpression (Pine.valueFromString "Literal")
                    , [ Pine.LiteralExpression (Pine.valueFromString elementName)

                      -- Below is the part we cannot express using a literal.
                      , expressionToLookupNameInEnvironment elementName
                      ]
                        |> Pine.ListExpression
                    ]
                        |> Pine.ListExpression
                        |> Ok
    in
    environmentElements
        |> List.map buildEnvironmentElementExpression
        |> Result.Extra.combine
        |> Result.map
            (\environmentElementsExpressions ->
                ( stack
                , Pine.ListExpression
                    [ Pine.LiteralExpression (Pine.valueFromString "List")
                    , environmentElementsExpressions
                        |> Pine.ListExpression
                    ]
                )
            )


emitReferenceExpression : String -> EmitStack -> Result String Pine.Expression
emitReferenceExpression name compilation =
    case
        compilation.appEnvElementsOrder
            |> List.indexedMap Tuple.pair
            |> List.filter (Tuple.second >> (==) name)
            |> List.head
            |> Maybe.map Tuple.first
    of
        Nothing ->
            Err
                ("Failed getting runtime index for '"
                    ++ name
                    ++ "'. "
                    ++ String.fromInt (List.length compilation.appEnvElementsOrder)
                    ++ " names on the current stack: "
                    ++ String.join ", " compilation.appEnvElementsOrder
                )

        Just runtimeIndex ->
            {-
               Ok
                   (Pine.DecodeAndEvaluateExpression
                       { expression =
                           listItemFromIndexExpression_Pine 1
                               (listItemFromIndexExpression_Pine runtimeIndex Pine.EnvironmentExpression)
                       , environment = Pine.EnvironmentExpression
                       }
                   )
            -}
            Ok
                (Pine.DecodeAndEvaluateExpression
                    { expression = expressionToLookupNameInEnvironment name
                    , environment = Pine.EnvironmentExpression
                    }
                )


getDeclarationValueFromCompilation : ( List String, String ) -> CompilationStack -> Result String Pine.Expression
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
            case Dict.get nameInModule moduleValue of
                Nothing ->
                    Err
                        ("Did not find '"
                            ++ nameInModule
                            ++ "' in module '"
                            ++ String.join "." canonicalModuleName
                            ++ "'. There are "
                            ++ String.fromInt (Dict.size moduleValue)
                            ++ " names available in that module: "
                            ++ String.join ", " (Dict.keys moduleValue)
                        )

                Just declarationValue ->
                    Ok declarationValue


expressionToLookupNameInEnvironment : String -> Pine.Expression
expressionToLookupNameInEnvironment name =
    expressionToLookupNameInGivenScope name Pine.EnvironmentExpression


expressionToLookupNameInGivenScope : String -> Pine.Expression -> Pine.Expression
expressionToLookupNameInGivenScope name scopeExpression =
    pineKernel_ListHead_Pine
        (applyKernelFunctionWithTwoArguments_Pine
            "look_up_name_in_ListValue"
            (Pine.LiteralExpression (Pine.valueFromString name))
            scopeExpression
        )


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
    [ ( "+", 0 )
    , ( "-", 0 )
    , ( "*", 1 )
    , ( "//", 1 )
    , ( "/", 1 )
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

                defaultCompilationStack =
                    { moduleAliases = Dict.empty
                    , availableModules = environmentDeclarations.modules
                    , availableDeclarations =
                        environmentDeclarations.otherDeclarations |> Dict.map (always CompiledDeclaration)
                    , inliningParentDeclarations = Set.empty
                    , elmValuesToExposeToGlobal = elmValuesToExposeToGlobalDefault
                    }

                emitStack =
                    { declarationsDependencies = Dict.empty
                    , appEnvElementsOrder = []
                    }
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
                                                |> Dict.insert
                                                    declarationName
                                                    (internalDeclarationFromFunction functionDeclaration)
                                    }
                            in
                            case
                                compileElmSyntaxFunction compilationStack functionDeclaration
                                    |> Result.map Tuple.second
                                    |> Result.andThen
                                        (\functionDeclarationCompilation ->
                                            emitClosureExpression
                                                emitStack
                                                [ ( declarationName, functionDeclarationCompilation ) ]
                                                |> (|>) functionDeclarationCompilation
                                        )
                            of
                                Err error ->
                                    Err ("Failed to compile Elm function declaration: " ++ error)

                                Ok declaredFunctionExpression ->
                                    let
                                        declarationValue =
                                            Pine.encodeExpressionAsValue declaredFunctionExpression
                                    in
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
                            Err "Custom type declaration as submission is not implemented"

                        Elm.Syntax.Declaration.PortDeclaration _ ->
                            Err "Port declaration as submission is not implemented"

                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                            Err "Infix declaration as submission is not implemented"

                        Elm.Syntax.Declaration.Destructuring _ _ ->
                            Err "Destructuring as submission is not implemented"

                Ok (ExpressionSubmission elmExpression) ->
                    case
                        compileElmSyntaxExpression defaultCompilationStack elmExpression
                            |> Result.andThen (emitExpression emitStack)
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


emitModuleValue : ElmModuleInCompilation -> Pine.Value
emitModuleValue =
    Dict.toList
        >> List.map (Tuple.mapSecond Pine.encodeExpressionAsValue)
        >> List.map Pine.valueFromContextExpansionWithName
        >> Pine.ListValue


separateEnvironmentDeclarations :
    Dict.Dict String Pine.Value
    ->
        Result
            String
            { modules : Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
            , otherDeclarations : Dict.Dict String Pine.Expression
            }
separateEnvironmentDeclarations environmentDeclarations =
    environmentDeclarations
        |> Dict.filter (stringStartsWithUpper >> always)
        |> Dict.toList
        |> List.map (Tuple.mapFirst (String.split "."))
        |> List.map
            (\( moduleName, moduleValue ) ->
                getDeclarationsFromEnvironment moduleValue
                    |> Result.andThen getDeclarationsExpressionsFromValues
                    |> Result.map (Tuple.pair moduleName)
                    |> Result.mapError ((++) ("Failed to get declarations from module " ++ String.join "." moduleName))
            )
        |> Result.Extra.combine
        |> Result.map Dict.fromList
        |> Result.andThen
            (\environmentBeforeModules ->
                environmentDeclarations
                    |> Dict.filter (stringStartsWithUpper >> not >> always)
                    |> getDeclarationsExpressionsFromValues
                    |> Result.map
                        (\otherDeclarations ->
                            { modules = environmentBeforeModules
                            , otherDeclarations = otherDeclarations
                            }
                        )
            )


getDeclarationsExpressionsFromValues :
    Dict.Dict String Pine.Value
    -> Result String (Dict.Dict String Pine.Expression)
getDeclarationsExpressionsFromValues =
    Dict.map (always Pine.decodeExpressionFromValue)
        >> Dict.toList
        >> List.map
            (\( name, decodeResult ) ->
                decodeResult
                    |> Result.map (Tuple.pair name)
                    |> Result.mapError ((++) ("Failed to decode expression for declaration " ++ name ++ ": "))
            )
        >> Result.Extra.combine
        >> Result.map Dict.fromList


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
                    let
                        defaultListEncoding _ =
                            Json.Encode.object
                                [ ( "List", Json.Encode.list (json_encode_pineValue_Internal dictionary) list ) ]
                    in
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
