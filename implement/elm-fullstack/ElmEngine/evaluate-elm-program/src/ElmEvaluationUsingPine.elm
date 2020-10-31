module ElmEvaluationUsingPine exposing (evaluateExpressionText)

import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import ElmEvaluation
import Json.Encode
import Pine exposing (PineExpression(..), PineValue(..))
import Result.Extra


evaluateExpressionText : String -> Result String Json.Encode.Value
evaluateExpressionText elmExpressionText =
    case parseElmExpressionString elmExpressionText of
        Err error ->
            Err ("Failed to map from Elm to Pine expression: " ++ error)

        Ok pineExpression ->
            case pineExpressionContextBaseForElm of
                Err error ->
                    Err ("Framework error: " ++ error)

                Ok expressionContext ->
                    case Pine.evaluatePineExpression expressionContext pineExpression of
                        Err error ->
                            Err ("Failed to evaluate Pine expression: " ++ error)

                        Ok pineValue ->
                            pineValueAsJson pineValue


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


parseElmExpressionString : String -> Result String PineExpression
parseElmExpressionString elmExpressionText =
    case ElmEvaluation.parseExpressionFromString elmExpressionText of
        Err error ->
            Err ("Failed to parse Elm syntax: " ++ error)

        Ok elmSyntax ->
            case pineExpressionFromElm elmSyntax of
                Err error ->
                    Err ("Failed to map from Elm to Pine: " ++ error)

                Ok ok ->
                    Ok ok


pineExpressionContextBaseForElm : Result String Pine.PineExpressionContext
pineExpressionContextBaseForElm =
    let
        compileElmCoreModulesResults =
            elmCoreModulesTexts
                |> List.map
                    (\moduleText ->
                        case ElmEvaluation.parseElmModuleText moduleText of
                            Err _ ->
                                Err ("Failed to parse module text: " ++ (moduleText |> String.left 100))

                            Ok file ->
                                let
                                    moduleName =
                                        file
                                            |> ElmEvaluation.moduleNameFromSyntaxFile
                                            |> Elm.Syntax.Node.value
                                            |> String.join "."

                                    declarations =
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

                                    declarationsValues =
                                        declarations
                                            |> List.filterMap Result.toMaybe
                                            |> List.map
                                                (\( declaredName, namedExpression ) ->
                                                    PineList
                                                        [ PineStringOrInteger declaredName
                                                        , PineExpressionValue namedExpression
                                                        ]
                                                )
                                in
                                Ok (Pine.pineValueFromContextExpansionWithName ( moduleName, PineList declarationsValues ))
                    )
    in
    case compileElmCoreModulesResults |> Result.Extra.combine of
        Err error ->
            Err ("Failed to compile elm core module: " ++ error)

        Ok elmCoreModules ->
            Ok
                { commonModel = elmCoreModules
                , provisionalArgumentStack = []
                }


elmCoreModulesTexts : List String
elmCoreModulesTexts =
    [ -- https://github.com/elm/core/blob/84f38891468e8e153fc85a9b63bdafd81b24664e/src/List.elm#L150-L157
      """
module List exposing (..)


foldl func acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            foldl func (func x acc) xs
"""
    ]


pineExpressionFromElm : Elm.Syntax.Expression.Expression -> Result String PineExpression
pineExpressionFromElm elmExpression =
    case elmExpression of
        Elm.Syntax.Expression.Literal literal ->
            Ok (PineLiteral (PineStringOrInteger literal))

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
    case pineExpressionFromElm (Elm.Syntax.Node.value (Elm.Syntax.Node.value function.declaration).expression) of
        Err error ->
            Err ("Failed to map expression in let function: " ++ error)

        Ok letFunctionExpression ->
            let
                mapArgumentsToOnlyNameResults =
                    (Elm.Syntax.Node.value function.declaration).arguments
                        |> List.map Elm.Syntax.Node.value
                        |> List.map
                            (\argumentPattern ->
                                case argumentPattern of
                                    Elm.Syntax.Pattern.VarPattern argumentName ->
                                        Ok argumentName

                                    _ ->
                                        Err "Only var pattern is implemented so far."
                            )
            in
            case mapArgumentsToOnlyNameResults |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to map function argument pattern: " ++ error)

                Ok argumentsNames ->
                    Ok
                        ( Elm.Syntax.Node.value (Elm.Syntax.Node.value function.declaration).name
                        , functionExpressionFromArgumentsNamesAndExpression argumentsNames letFunctionExpression
                        )


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
                                                            { function = PineFunctionOrValue "PineKernel.listFirstElement"
                                                            , arguments = [ expression ]
                                                            }
                                                      )
                                                    , ( unconsRightName
                                                      , PineApplication
                                                            { function = PineFunctionOrValue "List.drop"
                                                            , arguments =
                                                                [ PineLiteral (PineStringOrInteger "1")
                                                                , expression
                                                                ]
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
