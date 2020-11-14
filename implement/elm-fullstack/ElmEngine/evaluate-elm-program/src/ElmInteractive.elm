module ElmInteractive exposing
    ( InteractiveContext(..)
    , SubmissionResponse(..)
    , elmValueAsExpression
    , elmValueAsJson
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
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.Type
import Json.Encode
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


type SubmissionResponse
    = SubmissionResponseValue { value : ElmValue }
    | SubmissionResponseNoValue


type ElmValue
    = ElmList (List ElmValue)
    | ElmStringOrInteger String
    | ElmTag String (List ElmValue)
    | ElmRecord (List ( String, ElmValue ))


evaluateExpressionText : InteractiveContext -> String -> Result String Json.Encode.Value
evaluateExpressionText context elmExpressionText =
    submissionInInteractive context [] elmExpressionText
        |> Result.andThen
            (\submissionResponse ->
                case submissionResponse of
                    SubmissionResponseNoValue ->
                        Err "This submission does not evaluate to a value."

                    SubmissionResponseValue responseWithValue ->
                        Ok (elmValueAsJson responseWithValue.value)
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
                                    case Pine.evaluateExpression expressionContext pineExpression of
                                        Err error ->
                                            Err ("Failed to evaluate Pine expression: " ++ error)

                                        Ok pineValue ->
                                            case pineValueAsElmValue pineValue of
                                                Err error ->
                                                    Err ("Failed to encode as Elm value: " ++ error)

                                                Ok valueAsElmValue ->
                                                    Ok (SubmissionResponseValue { value = valueAsElmValue })


expandContextWithListOfInteractiveSubmissions : List String -> Pine.ExpressionContext -> Result String Pine.ExpressionContext
expandContextWithListOfInteractiveSubmissions submissions contextBefore =
    submissions
        |> List.foldl
            (\submission -> Result.andThen (expandContextWithInteractiveSubmission submission))
            (Ok contextBefore)


expandContextWithInteractiveSubmission : String -> Pine.ExpressionContext -> Result String Pine.ExpressionContext
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
                                |> Pine.addToContext [ Pine.valueFromContextExpansionWithName ( declaredName, Pine.ExpressionValue declaredFunctionExpression ) ]
                                |> Ok

                _ ->
                    Ok contextBefore

        _ ->
            Ok contextBefore


elmValueAsExpression : ElmValue -> String
elmValueAsExpression elmValue =
    case elmValue of
        ElmList list ->
            "[" ++ (list |> List.map elmValueAsExpression |> String.join ",") ++ "]"

        ElmStringOrInteger string ->
            string |> Json.Encode.string |> Json.Encode.encode 0

        ElmRecord fields ->
            "{ " ++ (fields |> List.map (\( fieldName, fieldValue ) -> fieldName ++ " = " ++ elmValueAsExpression fieldValue) |> String.join ", ") ++ " }"

        ElmTag tagName tagArguments ->
            tagName :: (tagArguments |> List.map elmValueAsExpression) |> String.join " "


elmValueAsJson : ElmValue -> Json.Encode.Value
elmValueAsJson elmValue =
    case elmValue of
        ElmStringOrInteger string ->
            Json.Encode.string string

        ElmList list ->
            Json.Encode.list elmValueAsJson list

        ElmRecord fields ->
            Json.Encode.list (\( fieldName, fieldValue ) -> Json.Encode.list identity [ Json.Encode.string fieldName, elmValueAsJson fieldValue ]) fields

        ElmTag tagName tagArguments ->
            Json.Encode.list identity [ Json.Encode.string tagName, Json.Encode.list elmValueAsJson tagArguments ]


pineValueAsElmValue : Pine.Value -> Result String ElmValue
pineValueAsElmValue pineValue =
    case pineValue of
        Pine.StringOrIntegerValue string ->
            -- TODO: Use type inference to distinguish between string and integer
            Ok (ElmStringOrInteger string)

        Pine.ListValue list ->
            case list |> List.map pineValueAsElmValue |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to combine list: " ++ error)

                Ok listValues ->
                    let
                        resultAsList =
                            Ok (ElmList listValues)

                        tryMapToRecordField possiblyRecordField =
                            case possiblyRecordField of
                                ElmList [ ElmStringOrInteger fieldName, fieldValue ] ->
                                    if not (stringStartsWithUpper fieldName) then
                                        Just ( fieldName, fieldValue )

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                    in
                    case listValues |> List.map (tryMapToRecordField >> Result.fromMaybe "") |> Result.Extra.combine of
                        Ok recordFields ->
                            let
                                recordFieldsNames =
                                    List.map Tuple.first recordFields
                            in
                            if recordFieldsNames /= [] && List.sort recordFieldsNames == recordFieldsNames then
                                Ok (ElmRecord recordFields)

                            else
                                resultAsList

                        Err _ ->
                            case listValues of
                                [ ElmStringOrInteger tagName, ElmList tagArguments ] ->
                                    if stringStartsWithUpper tagName then
                                        Ok (ElmTag tagName tagArguments)

                                    else
                                        resultAsList

                                _ ->
                                    resultAsList

        Pine.ExpressionValue _ ->
            Err "ExpressionValue"


pineExpressionContextForElmInteractive : InteractiveContext -> Result String Pine.ExpressionContext
pineExpressionContextForElmInteractive context =
    case elmCoreModulesTexts |> List.map (parseElmModuleTextIntoPineValue elmCoreModulesTexts) |> Result.Extra.combine of
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
            case
                contextModulesTexts
                    |> List.map (parseElmModuleTextIntoPineValue (elmCoreModulesTexts ++ contextModulesTexts))
                    |> Result.Extra.combine
            of
                Err error ->
                    Err ("Failed to compile elm module from context: " ++ error)

                Ok contextModules ->
                    let
                        modulesValues =
                            (contextModules ++ elmCoreModules)
                                |> List.map (Tuple.mapFirst (String.join "."))
                                |> List.map Pine.valueFromContextExpansionWithName
                    in
                    elmValuesToExposeToGlobal
                        |> List.foldl exposeFromElmModuleToGlobal
                            { commonModel = modulesValues
                            , provisionalArgumentStack = []
                            }
                        |> Ok


exposeFromElmModuleToGlobal : ( List String, String ) -> Pine.ExpressionContext -> Pine.ExpressionContext
exposeFromElmModuleToGlobal ( moduleName, nameInModule ) context =
    case Pine.lookUpNameInContext (moduleName ++ [ nameInModule ] |> String.join ".") context of
        Err _ ->
            context

        Ok ( valueFromName, _ ) ->
            { context | commonModel = Pine.valueFromContextExpansionWithName ( nameInModule, valueFromName ) :: context.commonModel }


parseElmModuleTextIntoPineValue : List String -> String -> Result String ( Elm.Syntax.ModuleName.ModuleName, Pine.Value )
parseElmModuleTextIntoPineValue allModulesTexts moduleText =
    parseElmModuleTextIntoNamedExports allModulesTexts moduleText
        |> Result.map (Tuple.mapSecond (List.map Pine.valueFromContextExpansionWithName >> Pine.ListValue))


parseElmModuleTextIntoNamedExports : List String -> String -> Result String ( Elm.Syntax.ModuleName.ModuleName, List ( String, Pine.Value ) )
parseElmModuleTextIntoNamedExports allModulesTexts moduleText =
    case parseElmModuleText moduleText of
        Err _ ->
            Err ("Failed to parse module text: " ++ (moduleText |> String.left 100))

        Ok file ->
            let
                otherModulesTexts =
                    allModulesTexts |> Set.fromList |> Set.remove moduleText

                moduleName =
                    Elm.Syntax.Node.value (moduleNameFromSyntaxFile file)

                valueForImportedModule : Elm.Syntax.ModuleName.ModuleName -> Result String Pine.Value
                valueForImportedModule importedModuleName =
                    otherModulesTexts
                        |> Set.toList
                        |> List.foldl
                            (\otherModuleText intermediateResult ->
                                if intermediateResult /= Nothing then
                                    intermediateResult

                                else
                                    case parseElmModuleTextIntoPineValue allModulesTexts otherModuleText of
                                        Err parseOtherModuleError ->
                                            Just (Err ("Failed to parse candidate for imported module: " ++ parseOtherModuleError))

                                        Ok ( otherModuleName, otherModuleExports ) ->
                                            if otherModuleName /= importedModuleName then
                                                Nothing

                                            else
                                                Just (Ok otherModuleExports)
                            )
                            Nothing
                        |> Maybe.withDefault (Err ("Did not find the module with name " ++ String.join "." importedModuleName))

                valuesFromImportsResults : List (Result String ( String, Pine.Value ))
                valuesFromImportsResults =
                    file.imports
                        |> List.map
                            (\importSyntax ->
                                let
                                    importedModuleName =
                                        Elm.Syntax.Node.value (Elm.Syntax.Node.value importSyntax).moduleName
                                in
                                valueForImportedModule importedModuleName
                                    |> Result.map (\importValue -> ( String.join "." importedModuleName, importValue ))
                            )
            in
            case valuesFromImportsResults |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to translate imports: " ++ error)

                Ok importsValues ->
                    let
                        globalExposingValues =
                            elmValuesToExposeToGlobal
                                |> List.map
                                    (\( sourceModuleName, exposedNameInModule ) ->
                                        ( exposedNameInModule
                                        , Pine.ExpressionValue
                                            (Pine.FunctionOrValueExpression
                                                (String.join "." (sourceModuleName ++ [ exposedNameInModule ]))
                                            )
                                        )
                                    )

                        declarationsResults =
                            file.declarations
                                |> List.map Elm.Syntax.Node.value
                                |> List.filterMap
                                    (\declaration ->
                                        case declaration of
                                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                                Just [ pineExpressionFromElmFunction functionDeclaration ]

                                            Elm.Syntax.Declaration.CustomTypeDeclaration customTypeDeclaration ->
                                                Just
                                                    (customTypeDeclaration.constructors |> List.map (Elm.Syntax.Node.value >> pineExpressionFromElmValueConstructor))

                                            _ ->
                                                Nothing
                                    )
                                |> List.concat
                    in
                    case declarationsResults |> Result.Extra.combine of
                        Err error ->
                            Err ("Failed to translate declaration: " ++ error)

                        Ok declarations ->
                            let
                                declarationsValues =
                                    declarations |> List.map (Tuple.mapSecond Pine.ExpressionValue)
                            in
                            Ok ( moduleName, declarationsValues ++ importsValues ++ globalExposingValues )


elmCoreModulesTexts : List String
elmCoreModulesTexts =
    [ """
module Basics exposing (..)


identity : a -> a
identity x =
    x


always : a -> b -> a
always a _ =
    a

"""
    , -- https://github.com/elm/core/blob/84f38891468e8e153fc85a9b63bdafd81b24664e/src/List.elm
      """
module List exposing (..)


import Maybe exposing (Maybe(..))


singleton : a -> List a
singleton value =
    [value]


repeat : Int -> a -> List a
repeat n value =
    repeatHelp [] n value


repeatHelp : List a -> Int -> a -> List a
repeatHelp result n value =
    if n <= 0 then
        result
    else
        repeatHelp (cons value result) (n - 1) value


range : Int -> Int -> List Int
range lo hi =
    rangeHelp lo hi []


rangeHelp : Int -> Int -> List Int -> List Int
rangeHelp lo hi list =
    if lo <= hi then
        rangeHelp lo (hi - 1) (cons hi list)
    else
        list


cons : a -> List a -> List a
cons element list =
    PineKernel.listCons element list


map : (a -> b) -> List a -> List b
map f xs =
    foldr (\\x acc -> cons (f x) acc) [] xs


foldl : (a -> b -> b) -> b -> List a -> b
foldl func acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            foldl func (func x acc) xs


foldr : (a -> b -> b) -> b -> List a -> b
foldr func acc list =
    foldl func acc (reverse list)


filter : (a -> Bool) -> List a -> List a
filter isGood list =
    foldr (\\x xs -> if isGood x then cons x xs else xs) [] list


length : List a -> Int
length xs =
    foldl (\\_ i -> i + 1) 0 xs


reverse : List a -> List a
reverse list =
    foldl cons [] list


member : a -> List a -> Bool
member x xs =
    any (\\a -> a == x) xs


any : (a -> Bool) -> List a -> Bool
any isOkay list =
    case list of
        [] ->
            False

        next :: xs ->
            if isOkay next then
                True

            else
                any isOkay xs


isEmpty : List a -> Bool
isEmpty xs =
    case xs of
        [] ->
            True

        _ ->
            False


head : List a -> Maybe a
head list =
    case list of
        x :: xs ->
            Just x

        [] ->
            Nothing


tail : List a -> Maybe (List a)
tail list =
    case list of
        x :: xs ->
            Just xs

        [] ->
            Nothing


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
    , """
module Maybe exposing (..)


type Maybe a
    = Just a
    | Nothing


withDefault : a -> Maybe a -> a
withDefault default maybe =
    case maybe of
        Just value -> value
        Nothing -> default


map : (a -> b) -> Maybe a -> Maybe b
map f maybe =
    case maybe of
        Just value ->
            Just (f value)

        Nothing ->
            Nothing


andThen : (a -> Maybe b) -> Maybe a -> Maybe b
andThen callback maybeValue =
    case maybeValue of
        Just value ->
            callback value

        Nothing ->
            Nothing

"""
    ]


elmValuesToExposeToGlobal : List ( List String, String )
elmValuesToExposeToGlobal =
    [ ( [ "Basics" ], "identity" )
    , ( [ "Basics" ], "always" )
    , ( [ "Maybe" ], "Nothing" )
    , ( [ "Maybe" ], "Just" )
    ]


pineExpressionFromElm : Elm.Syntax.Expression.Expression -> Result String Pine.Expression
pineExpressionFromElm elmExpression =
    case elmExpression of
        Elm.Syntax.Expression.Literal literal ->
            Ok (Pine.LiteralExpression (Pine.StringOrIntegerValue literal))

        Elm.Syntax.Expression.CharLiteral char ->
            Ok (Pine.LiteralExpression (Pine.StringOrIntegerValue (String.fromInt (Char.toCode char))))

        Elm.Syntax.Expression.Integer integer ->
            Ok (Pine.LiteralExpression (Pine.StringOrIntegerValue (String.fromInt integer)))

        Elm.Syntax.Expression.Negation negatedElmExpression ->
            case pineExpressionFromElm (Elm.Syntax.Node.value negatedElmExpression) of
                Err error ->
                    Err ("Failed to map negated expression: " ++ error)

                Ok negatedExpression ->
                    Ok (Pine.ApplicationExpression { function = Pine.FunctionOrValueExpression "PineKernel.negate", arguments = [ negatedExpression ] })

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            Ok (Pine.FunctionOrValueExpression (String.join "." (moduleName ++ [ localName ])))

        Elm.Syntax.Expression.Application application ->
            case application |> List.map (Elm.Syntax.Node.value >> pineExpressionFromElm) |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to map application elements: " ++ error)

                Ok applicationElements ->
                    case applicationElements of
                        appliedFunctionSyntax :: arguments ->
                            Ok (Pine.ApplicationExpression { function = appliedFunctionSyntax, arguments = arguments })

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
                            (Pine.ApplicationExpression
                                { function = Pine.FunctionOrValueExpression ("(" ++ operator ++ ")")
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
                                    Ok (Pine.IfBlockExpression condition expressionIfTrue expressionIfFalse)

        Elm.Syntax.Expression.LetExpression letBlock ->
            pineExpressionFromElmLetBlock letBlock

        Elm.Syntax.Expression.ParenthesizedExpression parenthesizedExpression ->
            pineExpressionFromElm (Elm.Syntax.Node.value parenthesizedExpression)

        Elm.Syntax.Expression.ListExpr listExpression ->
            listExpression
                |> List.map (Elm.Syntax.Node.value >> pineExpressionFromElm)
                |> Result.Extra.combine
                |> Result.map Pine.ListExpression

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            pineExpressionFromElmCaseBlock caseBlock

        Elm.Syntax.Expression.LambdaExpression lambdaExpression ->
            pineExpressionFromElmLambda lambdaExpression

        Elm.Syntax.Expression.RecordExpr recordExpr ->
            recordExpr |> List.map Elm.Syntax.Node.value |> pineExpressionFromElmRecord

        _ ->
            Err
                ("Unsupported type of expression: "
                    ++ (elmExpression |> Elm.Syntax.Expression.encode |> Json.Encode.encode 0)
                )


pineExpressionFromElmLetBlock : Elm.Syntax.Expression.LetBlock -> Result String Pine.Expression
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


pineExpressionFromLetBlockDeclarationsAndExpression : List ( String, Pine.Expression ) -> Pine.Expression -> Pine.Expression
pineExpressionFromLetBlockDeclarationsAndExpression declarations expression =
    declarations
        |> List.foldl
            (\declaration combinedExpr ->
                Pine.ContextExpansionWithNameExpression
                    (Tuple.mapSecond Pine.ExpressionValue declaration)
                    combinedExpr
            )
            expression


pineExpressionFromElmLetDeclaration : Elm.Syntax.Expression.LetDeclaration -> Result String ( String, Pine.Expression )
pineExpressionFromElmLetDeclaration declaration =
    case declaration of
        Elm.Syntax.Expression.LetFunction letFunction ->
            pineExpressionFromElmFunction letFunction

        Elm.Syntax.Expression.LetDestructuring _ _ ->
            Err "Destructuring in let block not implemented yet."


pineExpressionFromElmFunction : Elm.Syntax.Expression.Function -> Result String ( String, Pine.Expression )
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
    -> Result String Pine.Expression
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


functionExpressionFromArgumentsNamesAndExpression : List String -> Pine.Expression -> Pine.Expression
functionExpressionFromArgumentsNamesAndExpression argumentsNames expression =
    argumentsNames
        |> List.foldr
            (\argumentName prevExpression -> Pine.FunctionExpression argumentName prevExpression)
            expression


pineExpressionFromElmValueConstructor : Elm.Syntax.Type.ValueConstructor -> Result String ( String, Pine.Expression )
pineExpressionFromElmValueConstructor valueConstructor =
    let
        constructorName =
            Elm.Syntax.Node.value valueConstructor.name

        argumentsNames =
            valueConstructor.arguments |> List.indexedMap (\i _ -> "value_constructor_argument_" ++ String.fromInt i)
    in
    Ok
        ( constructorName
        , argumentsNames
            |> List.foldl
                (\argumentName prevExpression -> Pine.FunctionExpression argumentName prevExpression)
                (Pine.tagValueExpression constructorName (argumentsNames |> List.map Pine.FunctionOrValueExpression))
        )


pineExpressionFromElmCaseBlock : Elm.Syntax.Expression.CaseBlock -> Result String Pine.Expression
pineExpressionFromElmCaseBlock caseBlock =
    case pineExpressionFromElm (Elm.Syntax.Node.value caseBlock.expression) of
        Err error ->
            Err ("Failed to map case block expression: " ++ error)

        Ok expression ->
            case caseBlock.cases |> List.map (pineExpressionFromElmCaseBlockCase expression) |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to map case in case-of block: " ++ error)

                Ok cases ->
                    let
                        ifBlockFromCase deconstructedCase nextBlockExpression =
                            Pine.IfBlockExpression
                                deconstructedCase.conditionExpression
                                (pineExpressionFromLetBlockDeclarationsAndExpression
                                    deconstructedCase.declarations
                                    deconstructedCase.thenExpression
                                )
                                nextBlockExpression
                    in
                    Ok
                        (List.foldr
                            ifBlockFromCase
                            (Pine.FunctionOrValueExpression "Error in mapping of case-of block: No matching branch.")
                            cases
                        )


pineExpressionFromElmCaseBlockCase :
    Pine.Expression
    -> Elm.Syntax.Expression.Case
    -> Result String { conditionExpression : Pine.Expression, declarations : List ( String, Pine.Expression ), thenExpression : Pine.Expression }
pineExpressionFromElmCaseBlockCase caseBlockValueExpression ( elmPattern, elmExpression ) =
    case pineExpressionFromElm (Elm.Syntax.Node.value elmExpression) of
        Err error ->
            Err ("Failed to map case expression: " ++ error)

        Ok expressionAfterDeconstruction ->
            case Elm.Syntax.Node.value elmPattern of
                Elm.Syntax.Pattern.AllPattern ->
                    Ok
                        { conditionExpression = Pine.LiteralExpression Pine.trueValue
                        , declarations = []
                        , thenExpression = expressionAfterDeconstruction
                        }

                Elm.Syntax.Pattern.ListPattern [] ->
                    let
                        conditionExpression =
                            Pine.ApplicationExpression
                                { function = Pine.FunctionOrValueExpression "(==)"
                                , arguments =
                                    [ caseBlockValueExpression
                                    , Pine.ListExpression []
                                    ]
                                }
                    in
                    Ok
                        { conditionExpression = conditionExpression
                        , declarations = []
                        , thenExpression = expressionAfterDeconstruction
                        }

                Elm.Syntax.Pattern.UnConsPattern unconsLeft unconsRight ->
                    case ( Elm.Syntax.Node.value unconsLeft, Elm.Syntax.Node.value unconsRight ) of
                        ( Elm.Syntax.Pattern.VarPattern unconsLeftName, Elm.Syntax.Pattern.VarPattern unconsRightName ) ->
                            let
                                declarations =
                                    [ ( unconsLeftName
                                      , Pine.ApplicationExpression
                                            { function = Pine.FunctionOrValueExpression "PineKernel.listHead"
                                            , arguments = [ caseBlockValueExpression ]
                                            }
                                      )
                                    , ( unconsRightName
                                      , Pine.ApplicationExpression
                                            { function = Pine.FunctionOrValueExpression "PineKernel.listTail"
                                            , arguments = [ caseBlockValueExpression ]
                                            }
                                      )
                                    ]

                                conditionExpression =
                                    Pine.ApplicationExpression
                                        { function = Pine.FunctionOrValueExpression "not"
                                        , arguments =
                                            [ Pine.ApplicationExpression
                                                { function = Pine.FunctionOrValueExpression "PineKernel.equals"
                                                , arguments =
                                                    [ caseBlockValueExpression
                                                    , Pine.ApplicationExpression
                                                        { function = Pine.FunctionOrValueExpression "PineKernel.listTail"
                                                        , arguments = [ caseBlockValueExpression ]
                                                        }
                                                    ]
                                                }
                                            ]
                                        }
                            in
                            Ok
                                { conditionExpression = conditionExpression
                                , declarations = declarations
                                , thenExpression = expressionAfterDeconstruction
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
                            Pine.ApplicationExpression
                                { function = Pine.FunctionOrValueExpression "PineKernel.equals"
                                , arguments =
                                    [ Pine.LiteralExpression (Pine.StringOrIntegerValue qualifiedName.name)
                                    , Pine.ApplicationExpression
                                        { function = Pine.FunctionOrValueExpression "PineKernel.listHead"
                                        , arguments = [ caseBlockValueExpression ]
                                        }
                                    ]
                                }
                    in
                    case mapArgumentsToOnlyNameResults |> Result.Extra.combine of
                        Err error ->
                            Err ("Failed to map pattern in case block: " ++ error)

                        Ok declarationsNames ->
                            let
                                listDropExpression listExpression numberToDrop =
                                    if numberToDrop < 1 then
                                        listExpression

                                    else
                                        listDropExpression
                                            (Pine.ApplicationExpression
                                                { function = Pine.FunctionOrValueExpression "PineKernel.listTail"
                                                , arguments = [ listExpression ]
                                                }
                                            )
                                            (numberToDrop - 1)

                                argumentFromIndexExpression argumentIndex =
                                    listDropExpression
                                        (Pine.ApplicationExpression
                                            { function = Pine.FunctionOrValueExpression "PineKernel.listHead"
                                            , arguments =
                                                [ Pine.ApplicationExpression
                                                    { function = Pine.FunctionOrValueExpression "PineKernel.listTail"
                                                    , arguments = [ caseBlockValueExpression ]
                                                    }
                                                ]
                                            }
                                        )
                                        argumentIndex

                                declarations =
                                    declarationsNames
                                        |> List.indexedMap
                                            (\argumentIndex declarationName ->
                                                ( declarationName
                                                , Pine.ApplicationExpression
                                                    { function = Pine.FunctionOrValueExpression "PineKernel.listHead"
                                                    , arguments = [ argumentFromIndexExpression argumentIndex ]
                                                    }
                                                )
                                            )
                            in
                            Ok
                                { conditionExpression = conditionExpression
                                , declarations = declarations
                                , thenExpression = expressionAfterDeconstruction
                                }

                _ ->
                    Err
                        ("Unsupported type of pattern in case-of block case: "
                            ++ Json.Encode.encode 0 (Elm.Syntax.Pattern.encode (Elm.Syntax.Node.value elmPattern))
                        )


pineExpressionFromElmLambda : Elm.Syntax.Expression.Lambda -> Result String Pine.Expression
pineExpressionFromElmLambda lambda =
    pineExpressionFromElmFunctionWithoutName
        { arguments = lambda.args |> List.map Elm.Syntax.Node.value
        , expression = Elm.Syntax.Node.value lambda.expression
        }


pineExpressionFromElmRecord : List Elm.Syntax.Expression.RecordSetter -> Result String Pine.Expression
pineExpressionFromElmRecord recordSetters =
    recordSetters
        |> List.map (Tuple.mapFirst Elm.Syntax.Node.value)
        |> List.sortBy Tuple.first
        |> List.map
            (\( fieldName, fieldExpressionNode ) ->
                case pineExpressionFromElm (Elm.Syntax.Node.value fieldExpressionNode) of
                    Err error ->
                        Err ("Failed to map record field: " ++ error)

                    Ok fieldExpression ->
                        Ok
                            (Pine.ListExpression
                                [ Pine.LiteralExpression (Pine.StringOrIntegerValue fieldName)
                                , fieldExpression
                                ]
                            )
            )
        |> Result.Extra.combine
        |> Result.map Pine.ListExpression


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


stringStartsWithUpper : String -> Bool
stringStartsWithUpper =
    String.uncons >> Maybe.map (Tuple.first >> Char.isUpper) >> Maybe.withDefault False
