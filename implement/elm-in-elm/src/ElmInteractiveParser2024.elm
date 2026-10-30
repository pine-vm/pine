module ElmInteractiveParser2024 exposing (..)

import Common
import ElmCompiler2024
    exposing
        ( InteractiveSubmission(..)
        , ProjectParsedElmFile
        )
import ElmInteractive2024
    exposing
        ( ElmCoreModulesExtent(..)
        , InteractiveContext(..)
        , SubmissionResponse
        , parsedElmFileRecordFromSeparatelyParsedSyntax
        , submissionResponseFromResponsePineValue
        )
import ElmInteractiveCoreModules
import ElmInteractiveKernelModules
import ElmInteractiveSubmissionParser2024
    exposing
        ( parseElmModuleText
        , parseInteractiveSubmissionFromString
        , parserDeadEndToString
        , parserDeadEndsToString
        )
import EncodeElmSyntaxAsPineValue2024
import Pine2024


compileEvalContextForElmInteractive : InteractiveContext -> Result String Pine2024.EvalEnvironment
compileEvalContextForElmInteractive context =
    let
        contextModulesTextsBatches : List (List String)
        contextModulesTextsBatches =
            case context of
                DefaultContext ->
                    [ ElmInteractiveCoreModules.elmCoreModulesTexts
                    , ElmInteractiveKernelModules.elmKernelModulesTexts
                    ]

                CustomModulesContext { includeCoreModules, modulesTexts } ->
                    [ case includeCoreModules of
                        Nothing ->
                            []

                        Just OnlyCoreModules ->
                            ElmInteractiveCoreModules.elmCoreModulesTexts

                        Just CoreAndOtherKernelModules ->
                            List.concat
                                [ ElmInteractiveCoreModules.elmCoreModulesTexts
                                , ElmInteractiveKernelModules.elmKernelModulesTexts
                                ]
                    , modulesTexts
                    ]
    in
    contextModulesTextsBatches
        |> List.foldl
            (\batchModuleTexts ->
                Result.andThen
                    (\prevEnvValue ->
                        expandElmInteractiveEnvironmentWithModuleTexts batchModuleTexts
                            |> Result.andThen
                                (\( _, addModules ) ->
                                    Result.map .environment (addModules prevEnvValue)
                                )
                    )
            )
            (Ok (Pine2024.ListValue []))
        |> Result.map Pine2024.EvalEnvironment


expandElmInteractiveEnvironmentWithModuleTexts :
    List String
    ->
        Result
            String
            ( List ProjectParsedElmFile
            , Pine2024.Value -> Result String { addedModules : List ( List String, Pine2024.Value ), environment : Pine2024.Value }
            )
expandElmInteractiveEnvironmentWithModuleTexts contextModulesTexts =
    contextModulesTexts
        |> Common.resultListMapCombine parsedElmFileFromOnlyFileText
        |> Result.map
            (\parsedModules ->
                ( parsedModules
                , \environmentBefore ->
                    ElmCompiler2024.expandElmInteractiveEnvironmentWithModules environmentBefore parsedModules
                )
            )


submissionInInteractive : InteractiveContext -> List String -> String -> Result String SubmissionResponse
submissionInInteractive context previousSubmissions submission =
    case compileEvalContextForElmInteractive context of
        Err error ->
            Err ("Failed to prepare the initial context: " ++ error)

        Ok initialContext ->
            submissionWithHistoryInInteractive initialContext previousSubmissions submission


submissionWithHistoryInInteractive : Pine2024.EvalEnvironment -> List String -> String -> Result String SubmissionResponse
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


submissionInInteractiveInPineContext : Pine2024.EvalEnvironment -> String -> Result String ( Pine2024.EvalEnvironment, SubmissionResponse )
submissionInInteractiveInPineContext (Pine2024.EvalEnvironment envValue) submission =
    compileInteractiveSubmission envValue submission
        |> Result.andThen
            (\pineExpression ->
                case Pine2024.evaluateExpression (Pine2024.EvalEnvironment envValue) pineExpression of
                    Err error ->
                        Err ("Failed to evaluate expression:\n" ++ Pine2024.displayStringFromPineError error)

                    Ok (Pine2024.BlobValue _) ->
                        Err "Type mismatch: Pine expression evaluated to a blob"

                    Ok (Pine2024.ListValue [ newState, responseValue ]) ->
                        submissionResponseFromResponsePineValue responseValue
                            |> Result.map (Tuple.pair (Pine2024.EvalEnvironment newState))

                    Ok (Pine2024.ListValue resultList) ->
                        Err
                            ("Type mismatch: Pine expression evaluated to a list with unexpected number of elements: "
                                ++ String.fromInt (List.length resultList)
                                ++ " instead of 2"
                            )
            )


{-| The expression evaluates to a list with two elements:
The first element contains the new interactive session state for the possible next submission.
The second element contains the response, the value to display to the user.
-}
compileInteractiveSubmission : Pine2024.Value -> String -> Result String Pine2024.Expression
compileInteractiveSubmission environment submission =
    case parseInteractiveSubmissionFromString submission of
        Err error ->
            Ok
                (ElmCompiler2024.buildExpressionForNewStateAndResponse
                    { newStateExpression = Pine2024.environmentExpr
                    , responseExpression =
                        ElmCompiler2024.responseExpressionFromString ("Failed to parse submission: " ++ error)
                    }
                )

        Ok parseOk ->
            ElmCompiler2024.compileParsedInteractiveSubmission environment parseOk


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
                (parsedElmFileRecordFromSeparatelyParsedSyntax
                    ( fileText, parsedModule )
                )


parseElmModuleTextToPineValue : String -> Result String Pine2024.Value
parseElmModuleTextToPineValue elmModule =
    case parseElmModuleText elmModule of
        Err deadEnds ->
            Err ("Failed to parse this as module text: " ++ parserDeadEndsToString deadEnds)

        Ok file ->
            case EncodeElmSyntaxAsPineValue2024.encodeElmSyntaxFileAsPineValue file of
                Err error ->
                    Err ("Failed to encode the parsed module as Pine value: " ++ error)

                Ok pineValue ->
                    Ok pineValue
