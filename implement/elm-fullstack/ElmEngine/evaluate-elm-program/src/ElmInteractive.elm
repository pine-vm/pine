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
import Json.Encode
import Maybe.Extra
import Parser
import Pine
import Result.Extra
import SHA256


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
    | ElmChar Char
    | ElmInteger BigInt.BigInt
    | ElmString String
    | ElmTag String (List ElmValue)
    | ElmRecord (List ( String, ElmValue ))


type alias ProjectParsedElmFile =
    { projectedModuleName : List String
    , fileText : String
    , parseResult : Result (List Parser.DeadEnd) Elm.Syntax.File.File
    }


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
            Err ("Failed to parse submission: " ++ error)

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
                                            Err ("Failed to evaluate expression:\n" ++ displayStringFromPineError error)

                                        Ok pineValue ->
                                            case pineValueAsElmValue pineValue of
                                                Err error ->
                                                    Err ("Failed to encode as Elm value: " ++ error)

                                                Ok valueAsElmValue ->
                                                    Ok (SubmissionResponseValue { value = valueAsElmValue })


displayStringFromPineError : Pine.PathDescription String -> String
displayStringFromPineError error =
    case error of
        Pine.DescribePathEnd end ->
            end

        Pine.DescribePathNode nodeDescription node ->
            nodeDescription ++ "\n" ++ prependAllLines "  " (displayStringFromPineError node)


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

        ElmInteger integer ->
            integer
                |> BigInt.toString
                |> String.toInt
                |> Maybe.map Json.Encode.int
                |> Maybe.withDefault (Json.Encode.string "Failed to encode integer")
                |> Json.Encode.encode 0

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


elmValueAsJson : ElmValue -> Json.Encode.Value
elmValueAsJson elmValue =
    case elmValue of
        ElmInteger integer ->
            integer
                |> BigInt.toString
                |> String.toInt
                |> Maybe.map Json.Encode.int
                |> Maybe.withDefault (Json.Encode.string "Failed to map from BigInt to Int")

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


pineValueAsElmValue : Pine.Value -> Result String ElmValue
pineValueAsElmValue pineValue =
    case pineValue of
        Pine.BlobValue blobValue ->
            case blobValue of
                [] ->
                    Ok (ElmString "")

                firstByte :: _ ->
                    if firstByte == 0 || firstByte == 0x80 then
                        blobValue
                            |> Pine.bigIntFromBlobValue
                            |> Result.map ElmInteger

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
                        case listValues |> List.map tryMapToChar |> Maybe.Extra.combine of
                            Just chars ->
                                chars |> String.fromList |> ElmString |> Ok

                            Nothing ->
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

                                            else
                                                Ok (ElmTag tagName tagArguments)

                                        else
                                            resultAsList

                                    _ ->
                                        resultAsList

        Pine.ExpressionValue _ ->
            Err "ExpressionValue"

        Pine.ClosureValue _ _ _ ->
            Err "ClosureValue"


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


pineExpressionContextForElmInteractive : InteractiveContext -> Result String Pine.ExpressionContext
pineExpressionContextForElmInteractive context =
    let
        contextModulesTexts =
            case context of
                DefaultContext ->
                    []

                InitContextFromApp { modulesTexts } ->
                    modulesTexts
    in
    (elmCoreModulesTexts ++ contextModulesTexts)
        |> List.map parsedElmFileFromOnlyFileText
        |> Result.Extra.combine
        |> Result.andThen
            (\parsedElmFiles ->
                parsedElmFiles
                    |> List.map
                        (\moduleToTranslate ->
                            parseElmModuleTextIntoPineValue parsedElmFiles moduleToTranslate
                                |> Result.mapError
                                    ((++)
                                        ("Failed to translate elm module '"
                                            ++ String.join "." moduleToTranslate.projectedModuleName
                                            ++ "': "
                                        )
                                    )
                        )
                    |> Result.Extra.combine
            )
        |> Result.map
            (\contextModules ->
                let
                    modulesValues =
                        contextModules
                            |> List.map (Tuple.mapFirst (String.join "."))
                            |> List.map Pine.valueFromContextExpansionWithName
                in
                elmValuesToExposeToGlobal
                    |> List.foldl exposeFromElmModuleToGlobal { commonModel = modulesValues }
            )


parsedElmFileFromOnlyFileText : String -> Result String ProjectParsedElmFile
parsedElmFileFromOnlyFileText fileText =
    case parseElmModuleText fileText of
        Err _ ->
            Err ("Failed to parse the module text: " ++ fileText)

        Ok parsedModule ->
            Ok
                { fileText = fileText
                , parseResult = Ok parsedModule
                , projectedModuleName = Elm.Syntax.Node.value (moduleNameFromSyntaxFile parsedModule)
                }


exposeFromElmModuleToGlobal : ( List String, String ) -> Pine.ExpressionContext -> Pine.ExpressionContext
exposeFromElmModuleToGlobal ( moduleName, nameInModule ) context =
    case Pine.lookUpNameAsStringInContext (moduleName ++ [ nameInModule ]) context of
        Err _ ->
            context

        Ok ( valueFromName, _ ) ->
            { context | commonModel = Pine.valueFromContextExpansionWithName ( nameInModule, valueFromName ) :: context.commonModel }


parseElmModuleTextIntoPineValue : List ProjectParsedElmFile -> ProjectParsedElmFile -> Result String ( Elm.Syntax.ModuleName.ModuleName, Pine.Value )
parseElmModuleTextIntoPineValue allModules moduleToTranslate =
    parseElmModuleTextIntoNamedExports allModules moduleToTranslate
        |> Result.map (Tuple.mapSecond (List.map Pine.valueFromContextExpansionWithName >> Pine.ListValue))


parseElmModuleTextIntoNamedExports : List ProjectParsedElmFile -> ProjectParsedElmFile -> Result String ( Elm.Syntax.ModuleName.ModuleName, List ( String, Pine.Value ) )
parseElmModuleTextIntoNamedExports allModules moduleToTranslate =
    case moduleToTranslate.parseResult of
        Err _ ->
            Err ("Failed to parse module '" ++ String.join "." moduleToTranslate.projectedModuleName ++ "'")

        Ok file ->
            let
                otherModules =
                    allModules |> List.filter ((/=) moduleToTranslate)

                moduleName =
                    Elm.Syntax.Node.value (moduleNameFromSyntaxFile file)

                valueForImportedModule : Elm.Syntax.ModuleName.ModuleName -> Result String Pine.Value
                valueForImportedModule importedModuleName =
                    otherModules
                        |> List.foldl
                            (\otherModuleText intermediateResult ->
                                if intermediateResult /= Nothing then
                                    intermediateResult

                                else
                                    case parseElmModuleTextIntoPineValue allModules otherModuleText of
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
    , """
module Tuple exposing
  ( pair
  , first, second
  , mapFirst, mapSecond, mapBoth
  )


pair : a -> b -> (a, b)
pair a b =
  (a, b)


first : (a, b) -> a
first (x,_) =
    x


second : (a, b) -> b
second (_,y) =
    y


mapFirst : (a -> x) -> (a, b) -> (x, b)
mapFirst func (x,y) =
    (func x, y)


mapSecond : (b -> y) -> (a, b) -> (a, y)
mapSecond func (x,y) =
    (x, func y)


mapBoth : (a -> x) -> (b -> y) -> (a, b) -> (x, y)
mapBoth funcA funcB (x,y) =
    ( funcA x, funcB y )

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

        x :: xs ->
            if isOkay x then
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


take : Int -> List a -> List a
take n list =
    if n < 1 then
        []

    else
        case list of
        [] ->
            []

        nextElement :: remainingElements ->
            [ nextElement ] ++ (take (n - 1) remainingElements)


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
    -- Add the sign prefix byte
    PineKernel.blobValueOneByteZero ++ char

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
    , """
module String exposing (..)

import Basics exposing (..)
import Char
import List exposing ((::))
import Maybe exposing (Maybe)


type alias String =
    List Char.Char


isEmpty : String -> Bool
isEmpty string =
    string == ""


length : String -> Int
length =
    List.length


reverse : String -> String
reverse =
    List.reverse


repeat : Int -> String -> String
repeat n chunk =
    if n < 1 then
        []

    else
        chunk ++ repeat (n - 1) chunk


replace : String -> String -> String -> String
replace before after string =
    join after (split before string)


append : String -> String -> String
append a b =
    a ++ b


concat : List String -> String
concat strings =
    join [] strings


split : String -> String -> List String
split =
    splitHelper []


splitHelper : String -> String -> String -> List String
splitHelper current sep string =
    if string == [] then
        [ current ]

    else if left (length sep) string == sep then
        [ current ] ++ splitHelper [] sep (dropLeft (length sep) string)

    else
        splitHelper (current ++ left 1 string) sep (dropLeft 1 string)


join : String -> List String -> String
join sep chunks =
    case chunks of
        [] ->
            []

        nextChunk :: remaining ->
            if remaining == []
            then
                nextChunk
            else
                nextChunk ++ sep ++ join sep remaining


slice : Int -> Int -> String -> String
slice start end string =
    let
        absoluteIndex relativeIndex =
            if relativeIndex < 0 then
                relativeIndex + length string

            else
                relativeIndex

        absoluteStart =
            absoluteIndex start
    in
    left (absoluteIndex end - absoluteStart) (List.drop absoluteStart string)


left : Int -> String -> String
left n string =
    List.take n string


dropLeft : Int -> String -> String
dropLeft n string =
    List.drop n string

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
            Ok (Pine.LiteralExpression (Pine.valueFromString literal))

        Elm.Syntax.Expression.CharLiteral char ->
            Ok (Pine.LiteralExpression (Pine.valueFromChar char))

        Elm.Syntax.Expression.Integer integer ->
            Ok (Pine.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt integer)))

        Elm.Syntax.Expression.Negation negatedElmExpression ->
            case pineExpressionFromElm (Elm.Syntax.Node.value negatedElmExpression) of
                Err error ->
                    Err ("Failed to map negated expression: " ++ error)

                Ok negatedExpression ->
                    Ok (Pine.ApplicationExpression { function = Pine.FunctionOrValueExpression "PineKernel.negate", arguments = [ negatedExpression ] })

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            Ok
                (pineValueFromFunctionOrValue moduleName localName
                    |> Maybe.map Pine.LiteralExpression
                    |> Maybe.withDefault (Pine.FunctionOrValueExpression (String.join "." (moduleName ++ [ localName ])))
                )

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

        Elm.Syntax.Expression.TupledExpression tupleElements ->
            tupleElements
                |> List.map (Elm.Syntax.Node.value >> pineExpressionFromElm)
                |> Result.Extra.combine
                |> Result.map Pine.ListExpression

        _ ->
            Err
                ("Unsupported type of expression: "
                    ++ (elmExpression |> Elm.Syntax.Expression.encode |> Json.Encode.encode 0)
                )


pineValueFromFunctionOrValue : List String -> String -> Maybe Pine.Value
pineValueFromFunctionOrValue moduleName nameInModule =
    if moduleName == [ "PineKernel" ] then
        case nameInModule of
            "blobValueOneByteZero" ->
                Just (Pine.BlobValue [ 0 ])

            _ ->
                Nothing

    else
        Nothing


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

        Ok functionBodyExpression ->
            case
                function.arguments
                    |> List.map
                        (\pattern ->
                            declarationsFromPattern pattern
                                |> Result.map (\deconstruct -> ( deconstruct, inspectionSymbolFromPattern pattern ))
                        )
                    |> Result.Extra.combine
            of
                Err error ->
                    Err ("Failed to map function argument pattern: " ++ error)

                Ok argumentsDeconstructDeclarationsBuilders ->
                    let
                        functionId =
                            function.expression
                                |> Elm.Syntax.Expression.encode
                                |> Json.Encode.encode 0
                                |> SHA256.fromString
                                |> SHA256.toHex
                                |> String.left 8

                        argumentsDeconstructionDeclarations =
                            argumentsDeconstructDeclarationsBuilders
                                |> List.indexedMap
                                    (\argIndex ( deconstruction, inspectionSymbol ) ->
                                        let
                                            argumentNameBeforeDeconstruct =
                                                String.join "_" [ "function", functionId, "argument", String.fromInt argIndex, inspectionSymbol ]
                                        in
                                        ( argumentNameBeforeDeconstruct
                                        , deconstruction (Pine.FunctionOrValueExpression argumentNameBeforeDeconstruct)
                                        )
                                    )

                        letBlockExpression =
                            pineExpressionFromLetBlockDeclarationsAndExpression
                                (List.concatMap Tuple.second argumentsDeconstructionDeclarations)
                                functionBodyExpression
                    in
                    Ok
                        (functionExpressionFromArgumentsNamesAndExpression
                            (argumentsDeconstructionDeclarations |> List.map Tuple.first)
                            letBlockExpression
                        )


inspectionSymbolFromPattern : Elm.Syntax.Pattern.Pattern -> String
inspectionSymbolFromPattern pattern =
    case pattern of
        Elm.Syntax.Pattern.AllPattern ->
            "ignored"

        Elm.Syntax.Pattern.VarPattern name ->
            name

        _ ->
            "other_pattern"


declarationsFromPattern : Elm.Syntax.Pattern.Pattern -> Result String (Pine.Expression -> List ( String, Pine.Expression ))
declarationsFromPattern pattern =
    case pattern of
        Elm.Syntax.Pattern.VarPattern varName ->
            Ok (\deconstructedExpression -> [ ( varName, deconstructedExpression ) ])

        Elm.Syntax.Pattern.AllPattern ->
            Ok (always [])

        Elm.Syntax.Pattern.TuplePattern tupleElements ->
            let
                getTupleElementExpression tupleElementIndex tupleExpression =
                    Pine.ApplicationExpression
                        { function = Pine.FunctionOrValueExpression "PineKernel.listHead"
                        , arguments = [ listDropExpression tupleElementIndex tupleExpression ]
                        }
            in
            case
                tupleElements
                    |> List.map Elm.Syntax.Node.value
                    |> List.map declarationsFromPattern
                    |> Result.Extra.combine
            of
                Err error ->
                    Err ("Failed to map tuple element: " ++ error)

                Ok tupleElementsDeconstructions ->
                    Ok
                        (\deconstructedExpression ->
                            tupleElementsDeconstructions
                                |> List.indexedMap
                                    (\tupleElementIndex tupleElement ->
                                        tupleElement (getTupleElementExpression tupleElementIndex deconstructedExpression)
                                    )
                                |> List.concat
                        )

        _ ->
            Err ("Unsupported type of pattern: " ++ (pattern |> Elm.Syntax.Pattern.encode |> Json.Encode.encode 0))


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
                                    [ Pine.LiteralExpression (Pine.valueFromString qualifiedName.name)
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
                                argumentFromIndexExpression argumentIndex =
                                    listDropExpression
                                        argumentIndex
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


listDropExpression : Int -> Pine.Expression -> Pine.Expression
listDropExpression numberToDrop listExpression =
    if numberToDrop < 1 then
        listExpression

    else
        listDropExpression
            (numberToDrop - 1)
            (Pine.ApplicationExpression
                { function = Pine.FunctionOrValueExpression "PineKernel.listTail"
                , arguments = [ listExpression ]
                }
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
                                [ Pine.LiteralExpression (Pine.valueFromString fieldName)
                                , fieldExpression
                                ]
                            )
            )
        |> Result.Extra.combine
        |> Result.map (Pine.ListExpression >> List.singleton >> Pine.tagValueExpression elmRecordTypeTagName)


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


prependAllLines : String -> String -> String
prependAllLines prefix text =
    text
        |> String.lines
        |> List.map ((++) prefix)
        |> String.join "\n"
