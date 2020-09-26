module ElmEvaluation exposing (..)

import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Json.Encode
import Parser
import Result.Extra


type FunctionOrValue
    = StringValue String
    | FunctionValue EvaluationContextLocals String FunctionOrValue
    | ExpressionValue Elm.Syntax.Expression.Expression
    | ListValue (List FunctionOrValue)
    | IntegerValue Int
    | CoreFunction (EvaluationContextLocals -> Result String FunctionOrValue)


type alias EvaluationContext =
    { modules : Dict.Dict (List String) ModuleStructure
    , currentModule : Maybe ModuleStructure
    , locals : EvaluationContextLocals
    }


type ModuleStructure
    = ModuleFromSyntax Elm.Syntax.File.File
    | ModuleFromCore (Dict.Dict String FunctionOrValue)


type alias EvaluationContextLocals =
    Dict.Dict String FunctionOrValue


type InteractiveSubmission
    = ExpressionSubmission Elm.Syntax.Expression.Expression
    | DeclarationSubmission Elm.Syntax.Declaration.Declaration


type SubmissionResponse
    = SubmissionResponseValue { valueAsJsonString : String, typeText : String }
    | SubmissionResponseNoValue


withLocalsAdded : EvaluationContextLocals -> EvaluationContext -> EvaluationContext
withLocalsAdded localsToAdd context =
    { context | locals = localsToAdd |> Dict.union context.locals }


functionValueFromFunctionImplementation : Elm.Syntax.Expression.FunctionImplementation -> Result String FunctionOrValue
functionValueFromFunctionImplementation functionImplementation =
    functionValueFromArgumentsAndExpression
        { arguments = functionImplementation.arguments, expression = functionImplementation.expression }


functionValueFromArgumentsAndExpression :
    { arguments : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    }
    -> Result String FunctionOrValue
functionValueFromArgumentsAndExpression functionImplementation =
    let
        withArgumentAdded argument functionBefore =
            case argument of
                Elm.Syntax.Pattern.VarPattern varName ->
                    Ok (FunctionValue Dict.empty varName functionBefore)

                _ ->
                    Err "Type of pattern is not implemented yet."
    in
    functionImplementation.arguments
        |> List.map Elm.Syntax.Node.value
        |> List.foldr
            (\argument -> Result.andThen (withArgumentAdded argument))
            (Ok (ExpressionValue (Elm.Syntax.Node.value functionImplementation.expression)))


moduleNameFromSyntaxFile : Elm.Syntax.File.File -> Elm.Syntax.Node.Node (List String)
moduleNameFromSyntaxFile file =
    case Elm.Syntax.Node.value file.moduleDefinition of
        Elm.Syntax.Module.NormalModule normalModule ->
            normalModule.moduleName

        Elm.Syntax.Module.PortModule portModule ->
            portModule.moduleName

        Elm.Syntax.Module.EffectModule effectModule ->
            effectModule.moduleName


lookUpModule : EvaluationContext -> List String -> Maybe ModuleStructure
lookUpModule context moduleName =
    context.modules |> Dict.get moduleName


lookUpValueInModule : String -> ModuleStructure -> Maybe FunctionOrValue
lookUpValueInModule name module_ =
    case module_ of
        ModuleFromSyntax file ->
            file.declarations
                |> List.map Elm.Syntax.Node.value
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                let
                                    functionImplementation =
                                        Elm.Syntax.Node.value functionDeclaration.declaration
                                in
                                if Elm.Syntax.Node.value functionImplementation.name /= name then
                                    Nothing

                                else
                                    Result.toMaybe
                                        (functionValueFromFunctionImplementation functionImplementation)

                            _ ->
                                Nothing
                    )
                |> List.head

        ModuleFromCore moduleFromCore ->
            moduleFromCore |> Dict.get name


lookUpValueInContext : EvaluationContext -> String -> Result { availableLocals : List String } FunctionOrValue
lookUpValueInContext context name =
    case context.locals |> Dict.get name of
        Just local ->
            Ok local

        Nothing ->
            context.currentModule
                |> Maybe.andThen (lookUpValueInModule name)
                |> Result.fromMaybe { availableLocals = context.locals |> Dict.keys }


evaluateExpressionString : List String -> String -> Result String { valueAsJsonString : String, typeText : String }
evaluateExpressionString modulesTexts =
    evaluateSubmissionStringInInteractive modulesTexts []
        >> Result.andThen
            (\submissionResponse ->
                case submissionResponse of
                    SubmissionResponseValue value ->
                        Ok value

                    SubmissionResponseNoValue ->
                        Err "Unexpected response: No value"
            )


evaluateExpressionStringWithoutModules : String -> Result String { valueAsJsonString : String, typeText : String }
evaluateExpressionStringWithoutModules =
    evaluateExpressionString []


serializeFunctionOrValue : FunctionOrValue -> { valueAsJsonString : String, typeText : String }
serializeFunctionOrValue value =
    case value of
        StringValue string ->
            { valueAsJsonString = "\"" ++ string ++ "\"", typeText = "String" }

        IntegerValue integer ->
            { valueAsJsonString = integer |> String.fromInt, typeText = "Int" }

        FunctionValue _ argName _ ->
            { valueAsJsonString = "Error: Got FunctionValue (argName: " ++ argName ++ ")"
            , typeText = "Error: Got FunctionValue"
            }

        ExpressionValue _ ->
            { valueAsJsonString = "Error: Got ExpressionValue", typeText = "Error: Got ExpressionValue" }

        ListValue list ->
            let
                elements =
                    list |> List.map serializeFunctionOrValue

                elementTypeText =
                    elements |> List.head |> Maybe.map .typeText |> Maybe.withDefault "a"
            in
            { valueAsJsonString = "[" ++ String.join "," (elements |> List.map .valueAsJsonString) ++ "]"
            , typeText = "List " ++ elementTypeText
            }

        CoreFunction _ ->
            { valueAsJsonString = "Error: Got CoreFunction", typeText = "Error: Got CoreFunction" }


evaluateSubmissionStringInInteractive : List String -> List String -> String -> Result String SubmissionResponse
evaluateSubmissionStringInInteractive modulesTexts previousSubmissions expressionCode =
    case parseInteractiveSubmissionFromString expressionCode of
        Err parseError ->
            Err
                ([ "Failed to parse submission:"
                 , "Failed to parse expression:"
                 , parseError.asExpressionError
                 , "Failed to parse declaration:"
                 , parseError.asDeclarationError
                 ]
                    |> String.join "\n"
                )

        Ok (DeclarationSubmission _) ->
            Ok SubmissionResponseNoValue

        Ok (ExpressionSubmission expression) ->
            case modulesTexts |> List.map parseElmModuleText |> Result.Extra.combine of
                Err _ ->
                    Err "Failed to parse module text"

                Ok modules ->
                    let
                        context =
                            { modules =
                                modules
                                    |> List.map
                                        (\file ->
                                            ( file |> moduleNameFromSyntaxFile |> Elm.Syntax.Node.value
                                            , ModuleFromSyntax file
                                            )
                                        )
                                    |> Dict.fromList
                                    |> Dict.union (coreModules |> Dict.map (always ModuleFromCore))
                            , currentModule = Nothing
                            , locals = Dict.empty
                            }
                                |> withLocalsAdded (getLocalsFromInteractiveSubmissions previousSubmissions)
                    in
                    evaluateExpression context expression
                        |> Result.mapError (\error -> "Failed to evaluate expression '" ++ expressionCode ++ "': " ++ error)
                        |> Result.map (serializeFunctionOrValue >> SubmissionResponseValue)


getLocalsFromInteractiveSubmissions : List String -> Dict.Dict String FunctionOrValue
getLocalsFromInteractiveSubmissions submissions =
    let
        getMaybeDeclarationFromSubmission =
            parseDeclarationFromString >> Result.toMaybe
    in
    submissions
        |> List.filterMap getMaybeDeclarationFromSubmission
        |> List.filterMap
            (\declaration ->
                case declaration of
                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                        Result.toMaybe
                            (getExpressionBindingFromLetDeclaration
                                (Elm.Syntax.Expression.LetFunction functionDeclaration)
                            )

                    _ ->
                        Nothing
            )
        |> List.foldl
            (\binding locals -> locals |> Dict.insert binding.name (ExpressionValue binding.expression))
            Dict.empty


evaluateExpression : EvaluationContext -> Elm.Syntax.Expression.Expression -> Result String FunctionOrValue
evaluateExpression context expression =
    case expression of
        Elm.Syntax.Expression.Literal literal ->
            Ok (StringValue literal)

        Elm.Syntax.Expression.OperatorApplication operator direction leftExpr rightExpr ->
            let
                orderedExpression =
                    mapExpressionForOperatorPrecedence expression
            in
            if orderedExpression == expression then
                evaluateOperatorApplication
                    context
                    { operator = operator, direction = direction, leftExpr = leftExpr, rightExpr = rightExpr }
                    |> Result.mapError (\error -> "Failed to apply operator (" ++ operator ++ "): " ++ error)

            else
                evaluateExpression context orderedExpression

        Elm.Syntax.Expression.LetExpression letBlock ->
            case
                letBlock.declarations
                    |> List.map (Elm.Syntax.Node.value >> getExpressionBindingFromLetDeclaration)
                    |> Result.Extra.combine
            of
                Err error ->
                    Err ("Failed to get value bindings from declaration in let block: " ++ error)

                Ok bindings ->
                    let
                        letContext =
                            context
                                |> withLocalsAdded
                                    (bindings
                                        |> List.map
                                            (\binding -> ( binding.name, ExpressionValue binding.expression ))
                                        |> Dict.fromList
                                    )
                    in
                    letBlock.expression
                        |> Elm.Syntax.Node.value
                        |> evaluateExpression letContext

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            let
                nextContext =
                    if moduleName == [] then
                        context

                    else
                        { context | currentModule = lookUpModule context moduleName }
            in
            lookUpValueInContext nextContext localName
                |> Result.mapError
                    (\lookupError ->
                        "Failed to look up value for '"
                            ++ localName
                            ++ "'"
                            ++ (if moduleName == [] then
                                    ""

                                else
                                    " in module '" ++ (moduleName |> String.join ".") ++ "'"
                               )
                            ++ ". "
                            ++ (lookupError.availableLocals |> List.length |> String.fromInt)
                            ++ " locals available: "
                            ++ (lookupError.availableLocals |> String.join ", ")
                    )
                |> Result.andThen
                    (\boundValue ->
                        case boundValue of
                            ExpressionValue expressionValue ->
                                evaluateExpression nextContext expressionValue

                            _ ->
                                Ok boundValue
                    )

        Elm.Syntax.Expression.Application application ->
            case application of
                appliedFunctionSyntax :: arguments ->
                    case evaluateExpression context (Elm.Syntax.Node.value appliedFunctionSyntax) of
                        Err error ->
                            Err ("Failed to look up function: " ++ error)

                        Ok function ->
                            case arguments |> List.map (Elm.Syntax.Node.value >> evaluateExpression context) |> Result.Extra.combine of
                                Err error ->
                                    Err ("Failed to evaluate argument: " ++ error)

                                Ok argumentsValues ->
                                    evaluateApplication context function argumentsValues

                [] ->
                    Err "Invalid shape of application: Zero elements in the application list"

        Elm.Syntax.Expression.ListExpr listExpression ->
            case listExpression |> List.map (Elm.Syntax.Node.value >> evaluateExpression context) |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to evaluate list expression: " ++ error)

                Ok values ->
                    Ok (ListValue values)

        Elm.Syntax.Expression.Integer integer ->
            Ok (IntegerValue integer)

        Elm.Syntax.Expression.ParenthesizedExpression parenthesizedExpression ->
            evaluateExpression context (Elm.Syntax.Node.value parenthesizedExpression)

        Elm.Syntax.Expression.LambdaExpression lambda ->
            functionValueFromArgumentsAndExpression { arguments = lambda.args, expression = lambda.expression }

        Elm.Syntax.Expression.PrefixOperator prefixOperator ->
            case coreFunctionFromOperatorString |> Dict.get prefixOperator of
                Just coreFunction ->
                    Ok (coreFunctionWith2Arguments coreFunction)

                _ ->
                    Err ("PrefixOperator not implemented: (" ++ prefixOperator ++ ")")

        _ ->
            Err ("Unsupported type of expression: " ++ (expression |> Elm.Syntax.Expression.encode |> Json.Encode.encode 0))


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


evaluateOperatorApplication :
    EvaluationContext
    ->
        { operator : String
        , direction : Elm.Syntax.Infix.InfixDirection
        , leftExpr : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , rightExpr : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    -> Result String FunctionOrValue
evaluateOperatorApplication context { operator, leftExpr, rightExpr } =
    case leftExpr |> Elm.Syntax.Node.value |> evaluateExpression context of
        Err error ->
            Err ("Failed to evaluate left expression: " ++ error)

        Ok leftValue ->
            case rightExpr |> Elm.Syntax.Node.value |> evaluateExpression context of
                Err error ->
                    Err ("Failed to evaluate right expression: " ++ error)

                Ok rightValue ->
                    case operator of
                        ">>" ->
                            Ok
                                (FunctionValue context.locals
                                    "function_composition_argument_name"
                                    (ExpressionValue
                                        (Elm.Syntax.Expression.Application
                                            [ rightExpr
                                            , Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange
                                                (Elm.Syntax.Expression.Application
                                                    [ leftExpr
                                                    , Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange
                                                        (Elm.Syntax.Expression.FunctionOrValue [] "function_composition_argument_name")
                                                    ]
                                                )
                                            ]
                                        )
                                    )
                                )

                        _ ->
                            case coreFunctionFromOperatorString |> Dict.get operator of
                                Just coreFunction ->
                                    coreFunction leftValue rightValue

                                Nothing ->
                                    Err ("Unsupported type of operator: " ++ operator)


coreFunctionFromOperatorString : Dict.Dict String (FunctionOrValue -> FunctionOrValue -> Result String FunctionOrValue)
coreFunctionFromOperatorString =
    [ ( "++", evaluateOperationPlusPlus )
    , ( "+", evaluateOperationPlus )
    , ( "-", evaluateOperationMinus )
    , ( "*", evaluateOperationAsterisk )
    , ( "//", evaluateOperationSlashSlash )
    ]
        |> Dict.fromList


evaluateOperationPlusPlus : FunctionOrValue -> FunctionOrValue -> Result String FunctionOrValue
evaluateOperationPlusPlus leftValue rightValue =
    case ( leftValue, rightValue ) of
        ( StringValue leftString, StringValue rightString ) ->
            Ok (StringValue (leftString ++ rightString))

        ( ListValue leftString, ListValue rightString ) ->
            Ok (ListValue (leftString ++ rightString))

        _ ->
            Err "Found unsupported type of value in operands"


evaluateOperationPlus : FunctionOrValue -> FunctionOrValue -> Result String FunctionOrValue
evaluateOperationPlus leftValue rightValue =
    case ( leftValue, rightValue ) of
        ( IntegerValue leftInt, IntegerValue rightInt ) ->
            Ok (IntegerValue (leftInt + rightInt))

        _ ->
            Err "Found unsupported type of value in operands"


evaluateOperationMinus : FunctionOrValue -> FunctionOrValue -> Result String FunctionOrValue
evaluateOperationMinus leftValue rightValue =
    case ( leftValue, rightValue ) of
        ( IntegerValue leftInt, IntegerValue rightInt ) ->
            Ok (IntegerValue (leftInt - rightInt))

        _ ->
            Err "Found unsupported type of value in operands"


evaluateOperationAsterisk : FunctionOrValue -> FunctionOrValue -> Result String FunctionOrValue
evaluateOperationAsterisk leftValue rightValue =
    case ( leftValue, rightValue ) of
        ( IntegerValue leftInt, IntegerValue rightInt ) ->
            Ok (IntegerValue (leftInt * rightInt))

        _ ->
            Err "Found unsupported type of value in operands"


evaluateOperationSlashSlash : FunctionOrValue -> FunctionOrValue -> Result String FunctionOrValue
evaluateOperationSlashSlash leftValue rightValue =
    case ( leftValue, rightValue ) of
        ( IntegerValue leftInt, IntegerValue rightInt ) ->
            Ok (IntegerValue (leftInt // rightInt))

        _ ->
            Err "Found unsupported type of value in operands"


evaluateApplication : EvaluationContext -> FunctionOrValue -> List FunctionOrValue -> Result String FunctionOrValue
evaluateApplication context function arguments =
    case arguments of
        [] ->
            case function of
                ExpressionValue expressionValue ->
                    evaluateExpression context expressionValue

                FunctionValue functionContext paramName nextFunction ->
                    Ok (FunctionValue (functionContext |> Dict.union context.locals) paramName nextFunction)

                CoreFunction coreFunction ->
                    coreFunction context.locals

                StringValue _ ->
                    Ok function

                ListValue _ ->
                    Ok function

                IntegerValue _ ->
                    Ok function

        currentArgument :: remainingArguments ->
            case function of
                FunctionValue functionContext paramName nextFunction ->
                    let
                        contextWithParamBound =
                            context
                                |> withLocalsAdded functionContext
                                |> withLocalsAdded (Dict.singleton paramName currentArgument)
                    in
                    evaluateApplication
                        contextWithParamBound
                        nextFunction
                        remainingArguments

                StringValue _ ->
                    Err "Found unexpected value for first element in application: StringValue"

                IntegerValue _ ->
                    Err "Found unexpected value for first element in application: IntegerValue"

                ExpressionValue _ ->
                    Err "Found unexpected value for first element in application: ExpressionValue"

                ListValue _ ->
                    Err "Found unexpected value for first element in application: ListValue"

                CoreFunction _ ->
                    Err "Found unexpected value for first element in application: CoreFunction"


getExpressionBindingFromLetDeclaration :
    Elm.Syntax.Expression.LetDeclaration
    -> Result String { name : String, expression : Elm.Syntax.Expression.Expression }
getExpressionBindingFromLetDeclaration letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetDestructuring _ _ ->
            Err "LetDestructuring is not implemented yet."

        Elm.Syntax.Expression.LetFunction letFunction ->
            let
                functionDeclaration =
                    Elm.Syntax.Node.value letFunction.declaration
            in
            if functionDeclaration.arguments == [] then
                Ok
                    { name = Elm.Syntax.Node.value functionDeclaration.name
                    , expression = functionDeclaration.expression |> Elm.Syntax.Node.value
                    }

            else
                Err "Function with argument is not implemented yet."


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


coreModules : Dict.Dict (List String) (Dict.Dict String FunctionOrValue)
coreModules =
    let
        unwrapString generalArgument =
            case generalArgument of
                StringValue string ->
                    Ok string

                _ ->
                    Err "Unexpected type"

        unwrapListString generalArgument =
            case generalArgument of
                ListValue listValue ->
                    listValue |> List.map unwrapString |> Result.Extra.combine

                _ ->
                    Err "Unexpected type"
    in
    [ ( [ "String" ]
      , [ ( "length"
          , coreFunctionWith1Argument
                (unwrapString >> Result.map (String.length >> IntegerValue))
          )
        , ( "toLower"
          , coreFunctionWith1Argument
                (unwrapString >> Result.map (String.toLower >> StringValue))
          )
        , ( "trim"
          , coreFunctionWith1Argument
                (unwrapString >> Result.map (String.trim >> StringValue))
          )
        , ( "split"
          , coreFunctionWith2Arguments
                (\separatorArg stringArg ->
                    case ( separatorArg |> unwrapString, stringArg |> unwrapString ) of
                        ( Ok separator, Ok string ) ->
                            Ok (String.split separator string |> List.map StringValue |> ListValue)

                        _ ->
                            Err "Error unwrapping argument"
                )
          )
        , ( "join"
          , coreFunctionWith2Arguments
                (\separatorArg chunksArg ->
                    case ( separatorArg |> unwrapString, chunksArg |> unwrapListString ) of
                        ( Ok separator, Ok chunks ) ->
                            Ok (String.join separator chunks |> StringValue)

                        _ ->
                            Err "Error unwrapping argument"
                )
          )
        ]
            |> Dict.fromList
      )
    ]
        |> Dict.fromList


coreFunctionWith1Argument : (FunctionOrValue -> Result String FunctionOrValue) -> FunctionOrValue
coreFunctionWith1Argument functionWith1Argument =
    FunctionValue
        Dict.empty
        "coreFuncArg0"
        (CoreFunction
            (Dict.get "coreFuncArg0"
                >> Result.fromMaybe "Error in core function argument name"
                >> Result.andThen functionWith1Argument
            )
        )


coreFunctionWith2Arguments : (FunctionOrValue -> FunctionOrValue -> Result String FunctionOrValue) -> FunctionOrValue
coreFunctionWith2Arguments functionWith2Arguments =
    FunctionValue
        Dict.empty
        "coreFuncArg0"
        (FunctionValue
            Dict.empty
            "coreFuncArg1"
            (CoreFunction
                (\locals ->
                    case ( locals |> Dict.get "coreFuncArg0", locals |> Dict.get "coreFuncArg1" ) of
                        ( Just arg0, Just arg1 ) ->
                            functionWith2Arguments arg0 arg1

                        _ ->
                            Err "Error in core function argument name"
                )
            )
        )
