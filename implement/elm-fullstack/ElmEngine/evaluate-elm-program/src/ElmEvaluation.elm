module ElmEvaluation exposing (..)

import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Pattern
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


withLocalsAdded : EvaluationContextLocals -> EvaluationContext -> EvaluationContext
withLocalsAdded localsToAdd context =
    { context | locals = localsToAdd |> Dict.union context.locals }


functionValueFromFunctionImplementation : Elm.Syntax.Expression.FunctionImplementation -> Result String FunctionOrValue
functionValueFromFunctionImplementation functionImplementation =
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
    evaluateExpressionSyntax modulesTexts
        >> Result.map serializeFunctionOrValue


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

        FunctionValue _ _ _ ->
            { valueAsJsonString = "Error: Got FunctionValue", typeText = "Error: Got FunctionValue" }

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


evaluateExpressionSyntax : List String -> String -> Result String FunctionOrValue
evaluateExpressionSyntax modulesTexts expressionCode =
    case parseExpressionFromString expressionCode of
        Err parseError ->
            Err ("Failed to parse expression: " ++ parseError)

        Ok expression ->
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
                    in
                    evaluateExpression context expression
                        |> Result.mapError (\error -> "Failed to evaluate expression '" ++ expressionCode ++ "': " ++ error)


evaluateExpression : EvaluationContext -> Elm.Syntax.Expression.Expression -> Result String FunctionOrValue
evaluateExpression context expression =
    case expression of
        Elm.Syntax.Expression.Literal literal ->
            Ok (StringValue literal)

        Elm.Syntax.Expression.OperatorApplication operator direction leftExpr rightExpr ->
            case leftExpr |> Elm.Syntax.Node.value |> evaluateExpression context of
                Err error ->
                    Err ("Failed to evaluate left expression: " ++ error)

                Ok leftValue ->
                    case rightExpr |> Elm.Syntax.Node.value |> evaluateExpression context of
                        Err error ->
                            Err ("Failed to evaluate right expression: " ++ error)

                        Ok rightValue ->
                            case operator of
                                "++" ->
                                    case ( leftValue, rightValue ) of
                                        ( StringValue leftString, StringValue rightString ) ->
                                            Ok (StringValue (leftString ++ rightString))

                                        ( ListValue leftString, ListValue rightString ) ->
                                            Ok (ListValue (leftString ++ rightString))

                                        _ ->
                                            Err "Found unsupported type of value in operands"

                                "+" ->
                                    case ( leftValue, rightValue ) of
                                        ( IntegerValue leftInt, IntegerValue rightInt ) ->
                                            Ok (IntegerValue (leftInt + rightInt))

                                        _ ->
                                            Err "Found unsupported type of value in operands"

                                "-" ->
                                    case ( leftValue, rightValue ) of
                                        ( IntegerValue leftInt, IntegerValue rightInt ) ->
                                            Ok (IntegerValue (leftInt - rightInt))

                                        _ ->
                                            Err "Found unsupported type of value in operands"

                                "*" ->
                                    case ( leftValue, rightValue ) of
                                        ( IntegerValue leftInt, IntegerValue rightInt ) ->
                                            Ok (IntegerValue (leftInt * rightInt))

                                        _ ->
                                            Err "Found unsupported type of value in operands"

                                "//" ->
                                    case ( leftValue, rightValue ) of
                                        ( IntegerValue leftInt, IntegerValue rightInt ) ->
                                            Ok (IntegerValue (leftInt // rightInt))

                                        _ ->
                                            Err "Found unsupported type of value in operands"

                                _ ->
                                    Err ("Unsupported type of operator: " ++ operator)

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
                                    " in module " ++ (moduleName |> String.join ".")
                               )
                            ++ ". Available locals: "
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

        _ ->
            Err ("Unsupported type of expression: " ++ (expression |> Elm.Syntax.Expression.encode |> Json.Encode.encode 0))


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
        getArgumentString generalArgument =
            case generalArgument of
                StringValue string ->
                    Ok string

                _ ->
                    Err "Unexpected type"

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
    in
    [ ( [ "String" ]
      , [ ( "length"
          , coreFunctionWith1Argument
                (getArgumentString >> Result.map (String.length >> IntegerValue))
          )
        ]
            |> Dict.fromList
      )
    ]
        |> Dict.fromList
