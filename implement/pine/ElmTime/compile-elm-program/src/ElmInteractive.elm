module ElmInteractive exposing (..)

import BigInt
import Common
import Dict
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.ModuleName
import ElmCompiler
    exposing
        ( CompilationStack
        , ElmModuleInCompilation
        , ProjectParsedElmFile
        , compilationAndEmitStackFromModulesInCompilation
        , elmFloatTypeTagName
        , elmRecordTypeTagName
        , elmStringTypeTagName
        , stringStartsWithUpper
        )
import FirCompiler
    exposing
        ( EmitStack
        , Expression(..)
        )
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Pine
import Result.Extra
import Set


type InteractiveSubmission
    = ExpressionSubmission Elm.Syntax.Expression.Expression
    | DeclarationSubmission Elm.Syntax.Declaration.Declaration


type InteractiveContext
    = DefaultContext
    | CustomModulesContext { includeCoreModules : Maybe ElmCoreModulesExtent, modulesTexts : List String }


type ElmCoreModulesExtent
    = OnlyCoreModules
    | CoreAndOtherKernelModules


type alias SubmissionResponse =
    { displayText : String }


type ElmValue
    = ElmList (List ElmValue)
    | ElmChar Char
    | ElmInteger BigInt.BigInt
    | ElmString String
    | ElmTag String (List ElmValue)
    | ElmRecord (List ( String, ElmValue ))
    | ElmFloat BigInt.BigInt BigInt.BigInt
    | ElmInternal String


submissionResponseFromResponsePineValue : Pine.Value -> Result String SubmissionResponse
submissionResponseFromResponsePineValue responseValue =
    case pineValueAsElmValue responseValue of
        Err error ->
            Err ("Failed to encode as Elm value: " ++ error)

        Ok valueAsElmValue ->
            Ok { displayText = Tuple.first (renderAsElmExpression valueAsElmValue) }


renderAsElmExpression : ElmValue -> ( String, { needsParens : Bool } )
renderAsElmExpression elmValue =
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
                ( "(" ++ (list |> List.map (renderAsElmExpression >> Tuple.first) |> String.join ",") ++ ")"
                , { needsParens = False }
                )

            else
                ( "[" ++ (list |> List.map (renderAsElmExpression >> Tuple.first) |> String.join ",") ++ "]"
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
                                    fieldName ++ " = " ++ Tuple.first (renderAsElmExpression fieldValue)
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
                        :: (tagArguments |> List.map (renderAsElmExpression >> applyNeedsParens))
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
                                            |> List.map (renderAsElmExpression >> Tuple.first)
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
                                                ++ Tuple.first (renderAsElmExpression key)
                                                ++ ","
                                                ++ Tuple.first (renderAsElmExpression value)
                                                ++ ")"
                                        )
                                )
                            ++ "]"
                        , { needsParens = True }
                        )

        ElmFloat numerator denominator ->
            ( case elmFloatToFloat numerator denominator of
                Nothing ->
                    "Failed conversion of float"

                Just asFloat ->
                    String.fromFloat asFloat
            , { needsParens = False }
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

        ElmFloat numerator denominator ->
            case elmFloatToFloat numerator denominator of
                Nothing ->
                    Json.Encode.string "Failed conversion of float"

                Just asFloat ->
                    Json.Encode.float asFloat

        ElmInternal _ ->
            Json.Encode.string (Tuple.first (renderAsElmExpression elmValue))


elmFloatToFloat : BigInt.BigInt -> BigInt.BigInt -> Maybe Float
elmFloatToFloat numerator denominator =
    case String.toInt (BigInt.toString numerator) of
        Nothing ->
            Nothing

        Just numeratorInt ->
            case String.toInt (BigInt.toString denominator) of
                Nothing ->
                    Nothing

                Just denominatorInt ->
                    Just (toFloat numeratorInt / toFloat denominatorInt)


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
                            case Pine.parseExpressionFromValue pineValue of
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

                                                else if tagName == elmFloatTypeTagName then
                                                    case tagArguments of
                                                        [ ElmInteger numerator, ElmInteger denominator ] ->
                                                            Ok (ElmFloat numerator denominator)

                                                        _ ->
                                                            Err "Unexpected shape under Float tag"

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
        tryMapToRecordField : ElmValue -> Result String ( String, ElmValue )
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

                                ElmTag tagName arguments ->
                                    Err
                                        ("Unexpected type in field name value: Tag "
                                            ++ tagName
                                            ++ " with "
                                            ++ String.fromInt (List.length arguments)
                                            ++ " arguments."
                                        )

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


{-| Missing safety with regards to the relation between the string and the syntax representation!
The integrating function must ensure that these two match.
Parsing was separated 2023-11-05 with the goal to reduce the time to arrive at a more efficient implementation of the compiler:
Using a Pine-based execution allows to skip the work required when running the Elm code via a JavaScript engine.
-}
parsedElmFileRecordFromSeparatelyParsedSyntax : ( String, Elm.Syntax.File.File ) -> ProjectParsedElmFile
parsedElmFileRecordFromSeparatelyParsedSyntax ( fileText, parsedModule ) =
    { fileText = fileText
    , parsedModule = parsedModule
    }


compilationAndEmitStackFromInteractiveEnvironment :
    { modules : Dict.Dict Elm.Syntax.ModuleName.ModuleName ElmModuleInCompilation
    , otherDeclarations : List ( String, Pine.Value )
    }
    -> ( CompilationStack, EmitStack )
compilationAndEmitStackFromInteractiveEnvironment environmentDeclarations =
    let
        interactiveImplicitImportStatements =
            environmentDeclarations.modules
                |> Dict.keys
                |> List.map
                    (\moduleName ->
                        { canonicalModuleName = moduleName
                        , localModuleName = moduleName
                        , exposingList = Nothing
                        }
                    )

        ( defaultCompilationStack, emitStack ) =
            compilationAndEmitStackFromModulesInCompilation
                environmentDeclarations.modules
                { moduleAliases = Dict.empty
                , parsedImports = interactiveImplicitImportStatements
                , localTypeDeclarations = []
                , selfModuleName = []
                }

        compilationStack =
            { defaultCompilationStack
                | inlineableDeclarations =
                    List.concat
                        [ defaultCompilationStack.inlineableDeclarations
                        , List.map
                            (\( declName, declValue ) ->
                                ( declName
                                , ElmCompiler.applicableDeclarationFromConstructorExpression
                                    (LiteralExpression declValue)
                                )
                            )
                            environmentDeclarations.otherDeclarations
                        ]
            }
    in
    ( compilationStack
    , emitStack
    )


json_encode_pineValue : Dict.Dict String Pine.Value -> Pine.Value -> Json.Encode.Value
json_encode_pineValue dictionary value =
    let
        dicts =
            Dict.foldl
                (\entryName entryValue aggregate ->
                    case entryValue of
                        Pine.BlobValue blob ->
                            if List.length blob < 3 then
                                aggregate

                            else
                                { aggregate
                                    | blobDict = Dict.insert blob entryName aggregate.blobDict
                                }

                        Pine.ListValue list ->
                            if list == [] then
                                aggregate

                            else
                                let
                                    hash =
                                        pineListValueFastHash list

                                    assocList =
                                        Dict.get hash aggregate.listDict
                                            |> Maybe.withDefault []
                                            |> (::) ( list, entryName )
                                in
                                { aggregate
                                    | listDict = Dict.insert hash assocList aggregate.listDict
                                }
                )
                { blobDict = Dict.empty, listDict = Dict.empty }
                dictionary
    in
    json_encode_pineValue_Internal
        dicts
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
                    if list == [] then
                        jsonEncodeEmptyList

                    else
                        Json.Encode.object
                            [ ( "List"
                              , Json.Encode.list (json_encode_pineValue_Internal dictionary) list
                              )
                            ]

                continueWithoutReference () =
                    case Pine.stringFromListValue list of
                        Err _ ->
                            defaultListEncoding ()

                        Ok asString ->
                            Json.Encode.object
                                [ ( "ListAsString"
                                  , Json.Encode.string asString
                                  )
                                ]
            in
            if list == [] then
                defaultListEncoding ()

            else
                case Dict.get (pineListValueFastHash list) dictionary.listDict of
                    Nothing ->
                        continueWithoutReference ()

                    Just assocList ->
                        case Common.listFind (Tuple.first >> (==) list) assocList of
                            Nothing ->
                                continueWithoutReference ()

                            Just ( _, reference ) ->
                                Json.Encode.object
                                    [ ( "Reference"
                                      , Json.Encode.string reference
                                      )
                                    ]

        Pine.BlobValue blob ->
            let
                defaultBlobEncoding () =
                    Json.Encode.object
                        [ ( "Blob"
                          , Json.Encode.list Json.Encode.int blob
                          )
                        ]

                tryFindReference () =
                    case Dict.get blob dictionary.blobDict of
                        Just reference ->
                            Json.Encode.object
                                [ ( "Reference"
                                  , Json.Encode.string reference
                                  )
                                ]

                        Nothing ->
                            defaultBlobEncoding ()
            in
            case blob of
                [] ->
                    jsonEncodeEmptyBlob

                _ ->
                    if List.length blob < 3 then
                        case intFromBlobValueStrict blob of
                            Err _ ->
                                defaultBlobEncoding ()

                            Ok asInt ->
                                Json.Encode.object
                                    [ ( "BlobAsInt"
                                      , Json.Encode.int asInt
                                      )
                                    ]

                    else
                        tryFindReference ()


intFromBlobValueStrict : List Int -> Result String Int
intFromBlobValueStrict blobBytes =
    case blobBytes of
        [] ->
            Err "Empty blob does not encode an integer."

        [ _ ] ->
            Err "Blob with only one byte does not encode an integer."

        sign :: absFirst :: following ->
            let
                computeAbsValue () =
                    if following == [] then
                        Ok absFirst

                    else if absFirst == 0 then
                        Err "Avoid ambiguous leading zero."

                    else
                        case following of
                            [ b1 ] ->
                                Ok ((absFirst * 256) + b1)

                            [ b1, b2 ] ->
                                Ok ((absFirst * 65536) + (b1 * 256) + b2)

                            [ b1, b2, b3 ] ->
                                Ok ((absFirst * 16777216) + (b1 * 65536) + (b2 * 256) + b3)

                            [ b1, b2, b3, b4 ] ->
                                Ok ((absFirst * 4294967296) + (b1 * 16777216) + (b2 * 65536) + (b3 * 256) + b4)

                            [ b1, b2, b3, b4, b5 ] ->
                                Ok ((absFirst * 1099511627776) + (b1 * 4294967296) + (b2 * 16777216) + (b3 * 65536) + (b4 * 256) + b5)

                            _ ->
                                Err "Failed to map to int - unsupported number of bytes"
            in
            case sign of
                4 ->
                    computeAbsValue ()

                2 ->
                    case computeAbsValue () of
                        Err err ->
                            Err err

                        Ok absValue ->
                            if absValue == 0 then
                                Err "Avoid ambiguous negative zero."

                            else
                                Ok -absValue

                _ ->
                    Err ("Unexpected value for sign byte: " ++ String.fromInt sign)


jsonEncodeEmptyList : Json.Encode.Value
jsonEncodeEmptyList =
    Json.Encode.object
        [ ( "List"
          , Json.Encode.list identity []
          )
        ]


jsonEncodeEmptyBlob : Json.Encode.Value
jsonEncodeEmptyBlob =
    Json.Encode.object
        [ ( "Blob"
          , Json.Encode.list Json.Encode.int []
          )
        ]


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
            Common.resultListMapCombine
                (resolvePineValueReferenceToLiteralRecursive stack dictionary)
                list
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
                        case
                            resolvePineValueReferenceToLiteralRecursive
                                (Set.insert reference stack)
                                dictionary
                                foundEntry
                        of
                            Err ( errorStack, errorMessage ) ->
                                Err ( reference :: errorStack, errorMessage )

                            Ok resolvedValue ->
                                Ok resolvedValue


json_decode_pineValueDictionaryEntry : Json.Decode.Decoder ( String, PineValueSupportingReference )
json_decode_pineValueDictionaryEntry =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "value" json_decode_pineValueSupportingReference)


json_decode_pineValueApplyingDictionary : Dict.Dict String Pine.Value -> Json.Decode.Decoder Pine.Value
json_decode_pineValueApplyingDictionary dictionary =
    json_decode_pineValueGeneric
        { decodeListElement =
            Json.Decode.lazy
                (\_ ->
                    Json.Decode.map Tuple.first
                        (json_decode_pineValueWithDictionary dictionary)
                )
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
        , Json.Decode.field "BlobAsInt" Json.Decode.int
            |> Json.Decode.map (Pine.valueFromInt >> config.consLiteral)
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
    let
        calculateEntryHash : Pine.Value -> Int
        calculateEntryHash entry =
            case entry of
                Pine.BlobValue blob ->
                    71 * List.length blob

                Pine.ListValue innerList ->
                    7919 * List.length innerList
    in
    case list of
        [] ->
            8831

        [ entry ] ->
            calculateEntryHash entry * 31

        entry1 :: entry2 :: _ ->
            calculateEntryHash entry1 * 41 + calculateEntryHash entry2 * 47 + List.length list


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

        KernelApplicationExpression functionName input ->
            [ ( "KernelApplication"
              , Json.Encode.object
                    [ ( "function", Json.Encode.string functionName )
                    , ( "input", expressionAsJson input )
                    ]
              )
            ]

        ConditionalExpression condition falseBranch trueBranch ->
            [ ( "Conditional"
              , Json.Encode.object
                    [ ( "condition", expressionAsJson condition )
                    , ( "falseBranch", expressionAsJson falseBranch )
                    , ( "trueBranch", expressionAsJson trueBranch )
                    ]
              )
            ]

        ReferenceExpression moduleName name ->
            [ ( "Reference"
              , [ ( "moduleName", Json.Encode.list Json.Encode.string moduleName )
                , ( "name", Json.Encode.string name )
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

        DeclarationBlockExpression _ _ ->
            [ ( "DeclarationBlock"
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

        PineFunctionApplicationExpression _ _ ->
            [ ( "PineFunctionApplication"
              , Json.Encode.object []
              )
            ]
    )
        |> Json.Encode.object
