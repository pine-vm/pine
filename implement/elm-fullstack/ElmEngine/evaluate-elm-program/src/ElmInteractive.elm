module ElmInteractive exposing
    ( InteractiveContext(..)
    , SubmissionResponse(..)
    , evaluateExpressionText
    , parseElmModuleText
    , parseElmModuleTextToJson
    , submissionInInteractive
    )

import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Json.Encode
import Parser
import Pine exposing (PineExpression(..), PineValue(..))
import Result.Extra


type InteractiveSubmission
    = ExpressionSubmission Elm.Syntax.Expression.Expression
    | DeclarationSubmission Elm.Syntax.Declaration.Declaration


type InteractiveContext
    = DefaultContext
    | InitContextFromApp { modulesTexts : List String }


type SubmissionResponse
    = SubmissionResponseValue { valueAsJson : Json.Encode.Value }
    | SubmissionResponseNoValue


evaluateExpressionText : InteractiveContext -> String -> Result String Json.Encode.Value
evaluateExpressionText context elmExpressionText =
    submissionInInteractive context [] elmExpressionText
        |> Result.andThen
            (\submissionResponse ->
                case submissionResponse of
                    SubmissionResponseNoValue ->
                        Err "This submission does not evaluate to a value."

                    SubmissionResponseValue responseWithValue ->
                        Ok responseWithValue.valueAsJson
            )


submissionInInteractive : InteractiveContext -> List String -> String -> Result String SubmissionResponse
submissionInInteractive context previousSubmissions submission =
    case parseInteractiveSubmissionFromString submission of
        Err error ->
            Err ("Failed to parse submission: " ++ error.asExpressionError)

        Ok (DeclarationSubmission _) ->
            Ok SubmissionResponseNoValue

        Ok (ExpressionSubmission elmExpression) ->
            case pineExpressionFromElm elmExpression of
                Err error ->
                    Err ("Failed to map from Elm to Pine expression: " ++ error)

                Ok pineExpression ->
                    case pineExpressionContextForElmInteractive context of
                        Err error ->
                            Err ("Failed to prepare the initial context: " ++ error)

                        Ok initialContext ->
                            case expandContextWithListOfInteractiveSubmissions previousSubmissions initialContext of
                                Err error ->
                                    Err ("Failed to apply previous submissions: " ++ error)

                                Ok expressionContext ->
                                    case Pine.evaluatePineExpression expressionContext pineExpression of
                                        Err error ->
                                            Err ("Failed to evaluate Pine expression: " ++ error)

                                        Ok pineValue ->
                                            case pineValueAsJson pineValue of
                                                Err error ->
                                                    Err ("Failed to encode as JSON: " ++ error)

                                                Ok valueAsJson ->
                                                    Ok (SubmissionResponseValue { valueAsJson = valueAsJson })


expandContextWithListOfInteractiveSubmissions : List String -> Pine.PineExpressionContext -> Result String Pine.PineExpressionContext
expandContextWithListOfInteractiveSubmissions submissions contextBefore =
    submissions
        |> List.foldl
            (\submission -> Result.andThen (expandContextWithInteractiveSubmission submission))
            (Ok contextBefore)


expandContextWithInteractiveSubmission : String -> Pine.PineExpressionContext -> Result String Pine.PineExpressionContext
expandContextWithInteractiveSubmission submission contextBefore =
    case parseInteractiveSubmissionFromString submission of
        Ok (DeclarationSubmission elmDeclaration) ->
            case elmDeclaration of
                Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                    case pineExpressionFromElmFunction functionDeclaration of
                        Err error ->
                            Err ("Failed to translate Elm function declaration: " ++ error)

                        Ok ( declaredName, declaredFunctionExpression ) ->
                            contextBefore
                                |> Pine.addToContext [ Pine.pineValueFromContextExpansionWithName ( declaredName, PineExpressionValue declaredFunctionExpression ) ]
                                |> Ok

                _ ->
                    Ok contextBefore

        _ ->
            Ok contextBefore


pineValueAsJson : PineValue -> Result String Json.Encode.Value
pineValueAsJson pineValue =
    case pineValue of
        PineStringOrInteger string ->
            -- TODO: Use type inference to distinguish between string and integer
            Ok (string |> Json.Encode.string)

        PineList list ->
            list
                |> List.map pineValueAsJson
                |> Result.Extra.combine
                |> Result.mapError (\error -> "Failed to combine list: " ++ error)
                |> Result.map (Json.Encode.list identity)

        PineExpressionValue _ ->
            Err "PineExpressionValue"


pineExpressionContextForElmInteractive : InteractiveContext -> Result String Pine.PineExpressionContext
pineExpressionContextForElmInteractive context =
    case elmCoreModulesTexts |> List.map parseElmModuleTextIntoPineValue |> Result.Extra.combine of
        Err error ->
            Err ("Failed to compile elm core module: " ++ error)

        Ok elmCoreModules ->
            let
                contextModulesTexts =
                    case context of
                        DefaultContext ->
                            []

                        InitContextFromApp { modulesTexts } ->
                            modulesTexts
            in
            case contextModulesTexts |> List.map parseElmModuleTextIntoPineValue |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to compile elm module from context: " ++ error)

                Ok contextModules ->
                    Ok
                        { commonModel = contextModules ++ elmCoreModules
                        , provisionalArgumentStack = []
                        }


parseElmModuleTextIntoPineValue : String -> Result String PineValue
parseElmModuleTextIntoPineValue moduleText =
    case parseElmModuleText moduleText of
        Err _ ->
            Err ("Failed to parse module text: " ++ (moduleText |> String.left 100))

        Ok file ->
            let
                moduleName =
                    file
                        |> moduleNameFromSyntaxFile
                        |> Elm.Syntax.Node.value
                        |> String.join "."

                declarationsResults =
                    file.declarations
                        |> List.map Elm.Syntax.Node.value
                        |> List.filterMap
                            (\declaration ->
                                case declaration of
                                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                        Just (pineExpressionFromElmFunction functionDeclaration)

                                    _ ->
                                        Nothing
                            )
            in
            case declarationsResults |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to translate declaration: " ++ error)

                Ok declarations ->
                    let
                        declarationsValues =
                            declarations
                                |> List.map
                                    (\( declaredName, namedExpression ) ->
                                        PineList
                                            [ PineStringOrInteger declaredName
                                            , PineExpressionValue namedExpression
                                            ]
                                    )
                    in
                    Ok (Pine.pineValueFromContextExpansionWithName ( moduleName, PineList declarationsValues ))


elmCoreModulesTexts : List String
elmCoreModulesTexts =
    [ -- https://github.com/elm/core/blob/84f38891468e8e153fc85a9b63bdafd81b24664e/src/List.elm
      """
module List exposing (..)


import Char exposing (Char)


foldl : (a -> b -> b) -> b -> List a -> b
foldl func acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            foldl func (func x acc) xs


length : List a -> Int
length xs =
    foldl (\\_ i -> i + 1) 0 xs


drop : Int -> List a -> List a
drop n list =
    if n <= 0 then
        list

    else
        case list of
        [] ->
            list

        x :: xs ->
            drop (n - 1) xs

"""
    , """
module Char exposing (..)


type alias Char = Int


toCode : Char -> Int
toCode char =
    char

"""
    ]


pineExpressionFromElm : Elm.Syntax.Expression.Expression -> Result String PineExpression
pineExpressionFromElm elmExpression =
    case elmExpression of
        Elm.Syntax.Expression.Literal literal ->
            Ok (PineLiteral (PineStringOrInteger literal))

        Elm.Syntax.Expression.CharLiteral char ->
            Ok (PineLiteral (PineStringOrInteger (String.fromInt (Char.toCode char))))

        Elm.Syntax.Expression.Integer integer ->
            Ok (PineLiteral (PineStringOrInteger (String.fromInt integer)))

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            Ok (PineFunctionOrValue (String.join "." (moduleName ++ [ localName ])))

        Elm.Syntax.Expression.Application application ->
            case application |> List.map (Elm.Syntax.Node.value >> pineExpressionFromElm) |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to map application elements: " ++ error)

                Ok applicationElements ->
                    case applicationElements of
                        appliedFunctionSyntax :: arguments ->
                            Ok (PineApplication { function = appliedFunctionSyntax, arguments = arguments })

                        [] ->
                            Err "Invalid shape of application: Zero elements in the application list"

        Elm.Syntax.Expression.OperatorApplication operator _ leftExpr rightExpr ->
            let
                orderedElmExpression =
                    mapExpressionForOperatorPrecedence elmExpression
            in
            if orderedElmExpression /= elmExpression then
                pineExpressionFromElm orderedElmExpression

            else
                case
                    ( pineExpressionFromElm (Elm.Syntax.Node.value leftExpr)
                    , pineExpressionFromElm (Elm.Syntax.Node.value rightExpr)
                    )
                of
                    ( Ok left, Ok right ) ->
                        Ok
                            (PineApplication
                                { function = PineFunctionOrValue ("(" ++ operator ++ ")")
                                , arguments = [ left, right ]
                                }
                            )

                    _ ->
                        Err "Failed to map OperatorApplication left or right expression. TODO: Expand error details."

        Elm.Syntax.Expression.IfBlock elmCondition elmExpressionIfTrue elmExpressionIfFalse ->
            case pineExpressionFromElm (Elm.Syntax.Node.value elmCondition) of
                Err error ->
                    Err ("Failed to map Elm condition: " ++ error)

                Ok condition ->
                    case pineExpressionFromElm (Elm.Syntax.Node.value elmExpressionIfTrue) of
                        Err error ->
                            Err ("Failed to map Elm expressionIfTrue: " ++ error)

                        Ok expressionIfTrue ->
                            case pineExpressionFromElm (Elm.Syntax.Node.value elmExpressionIfFalse) of
                                Err error ->
                                    Err ("Failed to map Elm expressionIfFalse: " ++ error)

                                Ok expressionIfFalse ->
                                    Ok (PineIfBlock condition expressionIfTrue expressionIfFalse)

        Elm.Syntax.Expression.LetExpression letBlock ->
            pineExpressionFromElmLetBlock letBlock

        Elm.Syntax.Expression.ParenthesizedExpression parenthesizedExpression ->
            pineExpressionFromElm (Elm.Syntax.Node.value parenthesizedExpression)

        Elm.Syntax.Expression.ListExpr listExpression ->
            listExpression
                |> List.map (Elm.Syntax.Node.value >> pineExpressionFromElm)
                |> Result.Extra.combine
                |> Result.map PineListExpr

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            pineExpressionFromElmCaseBlock caseBlock

        Elm.Syntax.Expression.LambdaExpression lambdaExpression ->
            pineExpressionFromElmLambda lambdaExpression

        _ ->
            Err
                ("Unsupported type of expression: "
                    ++ (elmExpression |> Elm.Syntax.Expression.encode |> Json.Encode.encode 0)
                )


pineExpressionFromElmLetBlock : Elm.Syntax.Expression.LetBlock -> Result String PineExpression
pineExpressionFromElmLetBlock letBlock =
    let
        declarationsResults =
            letBlock.declarations
                |> List.map (Elm.Syntax.Node.value >> pineExpressionFromElmLetDeclaration)
    in
    case declarationsResults |> Result.Extra.combine of
        Err error ->
            Err ("Failed to map declaration in let block: " ++ error)

        Ok declarations ->
            case pineExpressionFromElm (Elm.Syntax.Node.value letBlock.expression) of
                Err error ->
                    Err ("Failed to map expression in let block: " ++ error)

                Ok expressionInExpandedContext ->
                    Ok (pineExpressionFromLetBlockDeclarationsAndExpression declarations expressionInExpandedContext)


pineExpressionFromLetBlockDeclarationsAndExpression : List ( String, PineExpression ) -> PineExpression -> PineExpression
pineExpressionFromLetBlockDeclarationsAndExpression declarations expression =
    declarations
        |> List.foldl
            (\declaration combinedExpr ->
                PineContextExpansionWithName
                    (Tuple.mapSecond PineExpressionValue declaration)
                    combinedExpr
            )
            expression


pineExpressionFromElmLetDeclaration : Elm.Syntax.Expression.LetDeclaration -> Result String ( String, PineExpression )
pineExpressionFromElmLetDeclaration declaration =
    case declaration of
        Elm.Syntax.Expression.LetFunction letFunction ->
            pineExpressionFromElmFunction letFunction

        Elm.Syntax.Expression.LetDestructuring _ _ ->
            Err "Destructuring in let block not implemented yet."


pineExpressionFromElmFunction : Elm.Syntax.Expression.Function -> Result String ( String, PineExpression )
pineExpressionFromElmFunction function =
    pineExpressionFromElmFunctionWithoutName
        { arguments = (Elm.Syntax.Node.value function.declaration).arguments |> List.map Elm.Syntax.Node.value
        , expression = Elm.Syntax.Node.value (Elm.Syntax.Node.value function.declaration).expression
        }
        |> Result.map
            (\functionWithoutName ->
                ( Elm.Syntax.Node.value (Elm.Syntax.Node.value function.declaration).name
                , functionWithoutName
                )
            )


pineExpressionFromElmFunctionWithoutName :
    { arguments : List Elm.Syntax.Pattern.Pattern, expression : Elm.Syntax.Expression.Expression }
    -> Result String PineExpression
pineExpressionFromElmFunctionWithoutName function =
    case pineExpressionFromElm function.expression of
        Err error ->
            Err ("Failed to map expression in let function: " ++ error)

        Ok letFunctionExpression ->
            let
                mapArgumentsToOnlyNameResults =
                    function.arguments
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
            in
            case mapArgumentsToOnlyNameResults |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to map function argument pattern: " ++ error)

                Ok argumentsNames ->
                    Ok (functionExpressionFromArgumentsNamesAndExpression argumentsNames letFunctionExpression)


functionExpressionFromArgumentsNamesAndExpression : List String -> PineExpression -> PineExpression
functionExpressionFromArgumentsNamesAndExpression argumentsNames expression =
    argumentsNames
        |> List.foldr
            (\argumentName prevExpression -> PineFunction argumentName prevExpression)
            expression


pineExpressionFromElmCaseBlock : Elm.Syntax.Expression.CaseBlock -> Result String PineExpression
pineExpressionFromElmCaseBlock caseBlock =
    case pineExpressionFromElm (Elm.Syntax.Node.value caseBlock.expression) of
        Err error ->
            Err ("Failed to map case block expression: " ++ error)

        Ok expression ->
            case caseBlock.cases |> List.map (Tuple.mapFirst Elm.Syntax.Node.value) of
                [ ( Elm.Syntax.Pattern.ListPattern [], emptyCaseElmExpression ), ( Elm.Syntax.Pattern.UnConsPattern unconsLeft unconsRight, nonEmptyCaseElmExpression ) ] ->
                    case pineExpressionFromElm (Elm.Syntax.Node.value emptyCaseElmExpression) of
                        Err error ->
                            Err ("Failed to translate emptyCaseElmExpression: " ++ error)

                        Ok emptyCaseExpression ->
                            case pineExpressionFromElm (Elm.Syntax.Node.value nonEmptyCaseElmExpression) of
                                Err error ->
                                    Err ("Failed to translate nonEmptyCaseElmExpression: " ++ error)

                                Ok nonEmptyCaseExpression ->
                                    case ( Elm.Syntax.Node.value unconsLeft, Elm.Syntax.Node.value unconsRight ) of
                                        ( Elm.Syntax.Pattern.VarPattern unconsLeftName, Elm.Syntax.Pattern.VarPattern unconsRightName ) ->
                                            let
                                                nonEmptyCaseDeclarations =
                                                    [ ( unconsLeftName
                                                      , PineApplication
                                                            { function = PineFunctionOrValue "PineKernel.listHead"
                                                            , arguments = [ expression ]
                                                            }
                                                      )
                                                    , ( unconsRightName
                                                      , PineApplication
                                                            { function = PineFunctionOrValue "PineKernel.listTail"
                                                            , arguments = [ expression ]
                                                            }
                                                      )
                                                    ]

                                                conditionExpression =
                                                    PineApplication
                                                        { function = PineFunctionOrValue "(==)"
                                                        , arguments =
                                                            [ expression
                                                            , PineListExpr []
                                                            ]
                                                        }
                                            in
                                            Ok
                                                (PineIfBlock
                                                    conditionExpression
                                                    emptyCaseExpression
                                                    (pineExpressionFromLetBlockDeclarationsAndExpression
                                                        nonEmptyCaseDeclarations
                                                        nonEmptyCaseExpression
                                                    )
                                                )

                                        _ ->
                                            Err "Unsupported shape of uncons pattern."

                _ ->
                    Err
                        ("Unsupported shape of cases in case block: "
                            ++ (caseBlock
                                    |> Elm.Syntax.Expression.CaseExpression
                                    |> Elm.Syntax.Expression.encode
                                    |> Json.Encode.encode 0
                               )
                        )


pineExpressionFromElmLambda : Elm.Syntax.Expression.Lambda -> Result String PineExpression
pineExpressionFromElmLambda lambda =
    pineExpressionFromElmFunctionWithoutName
        { arguments = lambda.args |> List.map Elm.Syntax.Node.value
        , expression = Elm.Syntax.Node.value lambda.expression
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
                mappedRightExpr =
                    Elm.Syntax.Node.Node (Elm.Syntax.Node.range rightExpr)
                        (mapExpressionForOperatorPrecedence (Elm.Syntax.Node.value rightExpr))
            in
            case Elm.Syntax.Node.value mappedRightExpr of
                Elm.Syntax.Expression.OperatorApplication rightOperator _ rightLeftExpr rightRightExpr ->
                    let
                        operatorPriority =
                            operatorPrecendencePriority |> Dict.get operator |> Maybe.withDefault 0

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
                        Elm.Syntax.Expression.OperatorApplication rightOperator
                            direction
                            (Elm.Syntax.Node.Node
                                (Elm.Syntax.Range.combine [ Elm.Syntax.Node.range leftExpr, Elm.Syntax.Node.range rightLeftExpr ])
                                (Elm.Syntax.Expression.OperatorApplication operator direction leftExpr rightLeftExpr)
                            )
                            rightRightExpr

                    else
                        Elm.Syntax.Expression.OperatorApplication operator direction leftExpr mappedRightExpr

                _ ->
                    Elm.Syntax.Expression.OperatorApplication operator direction leftExpr mappedRightExpr

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


operatorPrecendencePriority : Dict.Dict String Int
operatorPrecendencePriority =
    [ ( "+", 0 )
    , ( "-", 0 )
    , ( "*", 1 )
    , ( "//", 1 )
    , ( "/", 1 )
    ]
        |> Dict.fromList


parseInteractiveSubmissionFromString : String -> Result { asExpressionError : String, asDeclarationError : String } InteractiveSubmission
parseInteractiveSubmissionFromString submission =
    case parseExpressionFromString submission of
        Ok expression ->
            Ok (ExpressionSubmission expression)

        Err expressionErr ->
            case parseDeclarationFromString submission of
                Ok declaration ->
                    Ok (DeclarationSubmission declaration)

                Err declarationErr ->
                    Err { asExpressionError = expressionErr, asDeclarationError = declarationErr }


parseExpressionFromString : String -> Result String Elm.Syntax.Expression.Expression
parseExpressionFromString expressionCode =
    let
        indentedExpressionCode =
            expressionCode
                |> String.lines
                |> List.map ((++) "    ")
                |> String.join "\n"

        moduleText =
            """
module Main exposing (..)


wrapping_expression_in_function =
"""
                ++ indentedExpressionCode
                ++ """

"""
    in
    parseElmModuleText moduleText
        |> Result.mapError (always "Failed to parse module")
        |> Result.andThen
            (\file ->
                file.declarations
                    |> List.filterMap
                        (\declaration ->
                            case Elm.Syntax.Node.value declaration of
                                Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                    functionDeclaration
                                        |> .declaration
                                        |> Elm.Syntax.Node.value
                                        |> .expression
                                        |> Elm.Syntax.Node.value
                                        |> Just

                                _ ->
                                    Nothing
                        )
                    |> List.head
                    |> Result.fromMaybe "Failed to extract the wrapping function."
            )


parseDeclarationFromString : String -> Result String Elm.Syntax.Declaration.Declaration
parseDeclarationFromString declarationCode =
    let
        moduleText =
            """
module Main exposing (..)


"""
                ++ declarationCode
                ++ """

"""
    in
    parseElmModuleText moduleText
        |> Result.mapError (always "Failed to parse module")
        |> Result.andThen
            (\file ->
                file.declarations
                    |> List.map Elm.Syntax.Node.value
                    |> List.head
                    |> Result.fromMaybe "Failed to extract the wrapping function."
            )


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
