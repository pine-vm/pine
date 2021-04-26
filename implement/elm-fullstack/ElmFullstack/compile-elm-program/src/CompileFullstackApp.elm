module CompileFullstackApp exposing
    ( AppFiles
    , CompilationError(..)
    , ElmTypeStructure(..)
    , loweredForSourceFiles
    , loweredForSourceFilesAndJsonCoders
    , parseElmTypeText
    )

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.Signature
import Elm.Syntax.TypeAnnotation
import Json.Encode
import List
import Maybe
import Parser
import Regex
import Result.Extra
import SHA256
import Set


type alias AppFiles =
    Dict.Dict (List String) Bytes.Bytes


type CompilationError
    = MissingDependencyError DependencyKey
    | OtherCompilationError String


type DependencyKey
    = ElmMakeDependency ElmMakeRequestStructure


type alias ElmMakeRequestStructure =
    { files : AppFiles
    , entryPointFilePath : List String
    , outputType : ElmMakeOutputType
    , enableDebug : Bool
    }


type ElmMakeOutputType
    = ElmMakeOutputTypeHtml
    | ElmMakeOutputTypeJs


{-| This function returns an Err if the needed dependencies for ElmMake are not yet in the dictionary.
The integrating software can then perform the ElmMake, insert it into the dependencies dict and retry.
-}
asCompletelyLoweredElmApp : List ( DependencyKey, Bytes.Bytes ) -> AppFiles -> Result (List CompilationError) AppFiles
asCompletelyLoweredElmApp dependencies sourceFiles =
    Ok sourceFiles


loweredForSourceFilesAndJsonCoders : List String -> AppFiles -> Result String AppFiles
loweredForSourceFilesAndJsonCoders compilationInterfaceElmModuleNamePrefixes sourceFiles =
    loweredForSourceFiles compilationInterfaceElmModuleNamePrefixes sourceFiles
        |> Result.andThen
            (\filesAfterLoweringForSourceFiles ->
                loweredForJsonCoders compilationInterfaceElmModuleNamePrefixes filesAfterLoweringForSourceFiles
            )


loweredForSourceFiles : List String -> AppFiles -> Result String AppFiles
loweredForSourceFiles compilationInterfaceElmModuleNamePrefixes sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    (compilationInterfaceElmModuleNamePrefix ++ ".SourceFiles")
                    mapSourceFilesModuleText
                    files
            )
            (Ok sourceFiles)


loweredForJsonCoders : List String -> AppFiles -> Result String AppFiles
loweredForJsonCoders compilationInterfaceElmModuleNamePrefixes sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    (compilationInterfaceElmModuleNamePrefix ++ ".GenerateJsonCoders")
                    mapJsonCodersModuleText
                    files
            )
            (Ok sourceFiles)


sourceFileFunctionNameStart : String
sourceFileFunctionNameStart =
    "file"


functionNameFlagsSeparator : String
functionNameFlagsSeparator =
    "____"


mapJsonCodersModuleText : ( AppFiles, String ) -> Result String ( AppFiles, String )
mapJsonCodersModuleText ( sourceFiles, moduleText ) =
    case parseElmModuleText moduleText of
        Err error ->
            -- TODO: Consolidate branch to parse with `mapSourceFilesModuleText`
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText error)

        Ok parsedModule ->
            parsedModule.declarations
                -- TODO: Also share the 'map all functions' part with `mapJsonCodersModuleText`
                |> List.filterMap
                    (\declaration ->
                        case Elm.Syntax.Node.value declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                Just functionDeclaration

                            _ ->
                                Nothing
                    )
                |> listFoldlToAggregateResult
                    (\functionDeclaration previousAggregate ->
                        replaceJsonCodingFunctionAndAddSupportingSyntaxInModuleText
                            { functionDeclaration = functionDeclaration
                            , declaringModuleText = moduleText
                            }
                            previousAggregate
                            |> Result.mapError
                                (\replaceFunctionError ->
                                    "Failed to replace function '"
                                        ++ Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                        ++ "': "
                                        ++ replaceFunctionError
                                )
                    )
                    (Ok ( sourceFiles, moduleText ))


mapSourceFilesModuleText : ( AppFiles, String ) -> Result String ( AppFiles, String )
mapSourceFilesModuleText ( sourceFiles, moduleText ) =
    case parseElmModuleText moduleText of
        Err error ->
            -- TODO: Consolidate branch to parse with `mapJsonCodersModuleText`
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText error)

        Ok parsedModule ->
            parsedModule.declarations
                -- TODO: Also share the 'map all functions' part with `mapJsonCodersModuleText`
                -- Remember: The module to interface with git services will probably use similar functionality.
                |> List.filterMap
                    (\declaration ->
                        case Elm.Syntax.Node.value declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                Just
                                    (Elm.Syntax.Node.value
                                        (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                    )

                            _ ->
                                Nothing
                    )
                |> listFoldlToAggregateResult
                    (\functionName previousAggregate ->
                        replaceFunctionInSourceFilesModuleText
                            sourceFiles
                            { functionName = functionName }
                            previousAggregate
                            |> Result.mapError
                                (\replaceFunctionError ->
                                    "Failed to replace function '"
                                        ++ functionName
                                        ++ "': "
                                        ++ replaceFunctionError
                                )
                    )
                    (addImportInElmModuleText [ "Base64" ] moduleText)
                |> Result.map (Tuple.pair sourceFiles)


replaceJsonCodingFunctionAndAddSupportingSyntaxInModuleText : { functionDeclaration : Elm.Syntax.Expression.Function, declaringModuleText : String } -> ( AppFiles, String ) -> Result String ( AppFiles, String )
replaceJsonCodingFunctionAndAddSupportingSyntaxInModuleText { functionDeclaration, declaringModuleText } ( sourceFiles, moduleText ) =
    case functionDeclaration.signature of
        Nothing ->
            Err "Missing function signature"

        Just functionSignature ->
            case parseJsonCodingFunctionType declaringModuleText (Elm.Syntax.Node.value functionSignature) of
                Err error ->
                    Err ("Failed parsing json coding function type: " ++ error)

                Ok functionType ->
                    case
                        appWithSupportForCodingElmType
                            sourceFiles
                            { typeCanonicalName = functionType.typeCanonicalName, moduleText = moduleText }
                    of
                        Err error ->
                            Err ("Failed to add supporting coding functions: " ++ error)

                        Ok ( appFiles, withSupportingFunctions ) ->
                            let
                                functionName =
                                    Elm.Syntax.Node.value (Elm.Syntax.Node.value functionSignature).name

                                codeTypeExpression =
                                    if functionType.isDecoder then
                                        withSupportingFunctions.decodeFunctionName

                                    else
                                        withSupportingFunctions.encodeFunctionName

                                newFunctionBody =
                                    indentElmCodeLines 1 codeTypeExpression

                                mapFunctionDeclarationLines originalFunctionTextLines =
                                    [ originalFunctionTextLines |> List.take 1
                                    , [ functionName ++ " = ", newFunctionBody ]
                                    ]
                                        |> List.concat
                            in
                            case
                                addOrUpdateFunctionInElmModuleText
                                    { functionName = functionName
                                    , mapFunctionLines = Maybe.withDefault [] >> mapFunctionDeclarationLines
                                    }
                                    withSupportingFunctions.moduleText
                            of
                                Err error ->
                                    Err ("Failed to replace function text: " ++ error)

                                Ok newModuleText ->
                                    Ok ( appFiles, newModuleText )


{-| Returns a new version of the app files because it adds exposure of custom type tag constructors.
-}
appWithSupportForCodingElmType : AppFiles -> { typeCanonicalName : String, moduleText : String } -> Result String ( AppFiles, { moduleText : String, decodeFunctionName : String, encodeFunctionName : String } )
appWithSupportForCodingElmType appFiles { typeCanonicalName, moduleText } =
    let
        sourceModules =
            elmModulesDictFromAppFiles appFiles

        getExpressionsAndDependenciesForType : String -> Result String ResolveTypeResult
        getExpressionsAndDependenciesForType canonicalTypeName =
            jsonCodingResolveType
                { rootTypeText = canonicalTypeName
                , maybeExplicitSourceModuleName = Nothing
                , rootSourceModuleText = moduleText
                }
                sourceModules
    in
    case getExpressionsAndDependenciesForType typeCanonicalName of
        Err error ->
            Err ("Failed to get expressions: " ++ error)

        Ok elmTypeExpressionsAndDependencies ->
            case
                enumerateExpressionsResolvingAllDependencies
                    getExpressionsAndDependenciesForType
                    (Set.singleton elmTypeExpressionsAndDependencies.canonicalTypeText)
            of
                Err error ->
                    Err ("Failed to get transitive expressions and dependencies: " ++ error)

                Ok functionCodingExpressions ->
                    case
                        functionCodingExpressions
                            |> List.map
                                (\supportingCoding ->
                                    buildJsonCodingFunctionTexts
                                        { typeText = supportingCoding.elmType
                                        , encodeExpression = supportingCoding.expressions.encodeExpression
                                        , decodeExpression = supportingCoding.expressions.decodeExpression
                                        }
                                )
                            |> Result.Extra.combine
                    of
                        Err error ->
                            Err ("Failed to build supporting coding function text: " ++ error)

                        Ok supportingCodingFunctionsBuilds ->
                            let
                                selfModuleName =
                                    parseElmModuleText moduleText
                                        |> Result.toMaybe
                                        |> Maybe.map (.moduleDefinition >> Elm.Syntax.Node.value)
                                        |> Maybe.map Elm.Syntax.Module.moduleName
                                        |> Maybe.withDefault []

                                modulesToImportForCustomTypes =
                                    functionCodingExpressions
                                        |> List.map (.expressions >> .referencedModules)
                                        |> List.foldl Set.union Set.empty
                                        |> Set.toList
                                        |> List.map (String.split ".")

                                modulesToImport =
                                    [ [ "Base64" ]
                                    , [ "Dict" ]
                                    , [ "Set" ]
                                    , [ "Json", "Decode" ]
                                    , [ "Json", "Encode" ]
                                    , [ "Bytes" ]
                                    , [ "Bytes", "Decode" ]
                                    , [ "Bytes", "Encode" ]
                                    ]
                                        ++ modulesToImportForCustomTypes
                                        |> List.filter ((==) selfModuleName >> not)

                                {-
                                   var appFilesAfterExposingCustomTypesInModules =
                                       functionCodingExpressionsDict
                                       .Select(exprResult => exprResult.Key)
                                       .Aggregate(
                                           originalAppFiles,
                                           (partiallyUpdatedAppFiles, elmType) =>
                                           {
                                               {
                                                   var enclosingParenthesesMatch = Regex.Match(elmType.Trim(), @"^\(([^,^\)]+)\)$");

                                                   if (enclosingParenthesesMatch.Success)
                                                       elmType = enclosingParenthesesMatch.Groups[1].Value;
                                               }

                                               var qualifiedMatch = Regex.Match(elmType.Trim(), @"^(.+)\.([^\s^\.]+)(\s+[a-z][^\s^\.]*)*$");

                                               if (!qualifiedMatch.Success)
                                                   return partiallyUpdatedAppFiles;

                                               var moduleName = qualifiedMatch.Groups[1].Value;
                                               var localTypeName = qualifiedMatch.Groups[2].Value;

                                               var expectedFilePath = FilePathFromModuleName(moduleName);

                                               var moduleBefore =
                                                   partiallyUpdatedAppFiles
                                                   .FirstOrDefault(candidate => candidate.Key.SequenceEqual(expectedFilePath));

                                               if (moduleBefore.Value == null)
                                                   return partiallyUpdatedAppFiles;

                                               var moduleTextBefore = Encoding.UTF8.GetString(moduleBefore.Value.ToArray());

                                               var isCustomTypeMatch = Regex.Match(
                                                   moduleTextBefore,
                                                   @"^type\s+" + localTypeName + @"(\s+[a-z][^\s]*){0,}\s*=", RegexOptions.Multiline);

                                               if (!isCustomTypeMatch.Success)
                                                   return partiallyUpdatedAppFiles;

                                               var moduleText = CompileElm.ExposeCustomTypeAllTagsInElmModule(moduleTextBefore, localTypeName);

                                               return partiallyUpdatedAppFiles.SetItem(moduleBefore.Key, Encoding.UTF8.GetBytes(moduleText).ToImmutableList());
                                           });
                                -}
                                appFilesAfterExposingCustomTypesInModules =
                                    functionCodingExpressions
                                        |> List.map .elmType
                                        |> List.foldl exposeNameInElmModuleInAppFilesIfCustomType appFiles
                                        |> appendLineAndStringInLogFile ("Adding exposing syntax completed for " ++ (String.fromInt (List.length functionCodingExpressions) ++ " supporting types."))
                                        |> appendLineAndStringInLogFile ("modulesToImportForCustomTypes: " ++ (modulesToImportForCustomTypes |> List.map (String.join ".") |> String.join ", "))

                                specificSupportingCodingFunctions =
                                    supportingCodingFunctionsBuilds
                                        |> List.concatMap
                                            (\buildFunctionResult ->
                                                [ { functionName = buildFunctionResult.encodeFunction.name
                                                  , functionText = buildFunctionResult.encodeFunction.text
                                                  }
                                                , { functionName = buildFunctionResult.decodeFunction.name
                                                  , functionText = buildFunctionResult.decodeFunction.text
                                                  }
                                                ]
                                            )

                                supportingFunctions =
                                    specificSupportingCodingFunctions ++ generalSupportingFunctionsTexts

                                interfaceModuleWithSupportingFunctions =
                                    supportingFunctions
                                        |> listFoldlToAggregateResult
                                            (\supportingFunction ->
                                                addOrUpdateFunctionInElmModuleText
                                                    { functionName = supportingFunction.functionName
                                                    , mapFunctionLines = always [ supportingFunction.functionText ]
                                                    }
                                            )
                                            (Ok moduleText)
                            in
                            case
                                listFoldlToAggregateResult
                                    addImportInElmModuleText
                                    interfaceModuleWithSupportingFunctions
                                    modulesToImport
                            of
                                Err error ->
                                    Err ("Failed to add imports: " ++ error)

                                Ok moduleTextWithImportsAdded ->
                                    functionNamesAndTypeParametersFromTypeText
                                        elmTypeExpressionsAndDependencies.canonicalTypeText
                                        |> Result.map
                                            (\elmTypeCodingFunctionNames ->
                                                ( appFilesAfterExposingCustomTypesInModules
                                                , { moduleText = moduleTextWithImportsAdded
                                                  , decodeFunctionName = elmTypeCodingFunctionNames.decodeFunctionName
                                                  , encodeFunctionName = elmTypeCodingFunctionNames.encodeFunctionName
                                                  }
                                                )
                                            )



{-
   {
       var enclosingParenthesesMatch = Regex.Match(elmType.Trim(), @"^\(([^,^\)]+)\)$");

       if (enclosingParenthesesMatch.Success)
           elmType = enclosingParenthesesMatch.Groups[1].Value;
   }

   var qualifiedMatch = Regex.Match(elmType.Trim(), @"^(.+)\.([^\s^\.]+)(\s+[a-z][^\s^\.]*)*$");

   if (!qualifiedMatch.Success)
       return partiallyUpdatedAppFiles;

   var moduleName = qualifiedMatch.Groups[1].Value;
   var localTypeName = qualifiedMatch.Groups[2].Value;

   var expectedFilePath = FilePathFromModuleName(moduleName);

   var moduleBefore =
       partiallyUpdatedAppFiles
       .FirstOrDefault(candidate => candidate.Key.SequenceEqual(expectedFilePath));

   if (moduleBefore.Value == null)
       return partiallyUpdatedAppFiles;

   var moduleTextBefore = Encoding.UTF8.GetString(moduleBefore.Value.ToArray());

   var isCustomTypeMatch = Regex.Match(
       moduleTextBefore,
       @"^type\s+" + localTypeName + @"(\s+[a-z][^\s]*){0,}\s*=", RegexOptions.Multiline);

   if (!isCustomTypeMatch.Success)
       return partiallyUpdatedAppFiles;

   var moduleText = CompileElm.ExposeCustomTypeAllTagsInElmModule(moduleTextBefore, localTypeName);

   return partiallyUpdatedAppFiles.SetItem(moduleBefore.Key, Encoding.UTF8.GetBytes(moduleText).ToImmutableList());
-}


exposeNameInElmModuleInAppFilesIfCustomType : String -> AppFiles -> AppFiles
exposeNameInElmModuleInAppFilesIfCustomType name appFiles =
    let
        appFilesWithCaseDescription caseDescription =
            appFiles |> appendLineAndStringInLogFile ("Exposing name '" ++ name ++ "': " ++ caseDescription)
    in
    case
        Regex.fromString "^\\(([^,^\\)]+)\\)$"
            |> Maybe.andThen (\regex -> Regex.find regex name |> List.head)
            |> Maybe.andThen (.submatches >> List.head)
            |> Maybe.andThen identity
    of
        Just inEnclosingParentheses ->
            exposeNameInElmModuleInAppFilesIfCustomType inEnclosingParentheses appFiles

        Nothing ->
            case name |> String.split "." |> List.reverse of
                [] ->
                    appFilesWithCaseDescription "Skipped because it is empty."

                localTypeName :: moduleNameReversed ->
                    if moduleNameReversed == [] then
                        appFilesWithCaseDescription "Skipped because it is not qualified."

                    else
                        let
                            moduleName =
                                List.reverse moduleNameReversed

                            moduleFilePath =
                                filePathFromElmModuleName (String.join "." moduleName)
                        in
                        case
                            appFiles
                                |> Dict.get moduleFilePath
                                |> Maybe.andThen stringFromFileContent
                        of
                            Nothing ->
                                appFilesWithCaseDescription "Skipped because did not find the module."

                            Just originalModuleText ->
                                case
                                    Regex.fromString ("\ntype\\s+" ++ localTypeName ++ "(\\s+[a-z][^\\s]*){0,}\\s*\n\\s*=")
                                        |> Maybe.andThen
                                            (\regex -> Regex.find regex originalModuleText |> List.head)
                                of
                                    Nothing ->
                                        appFilesWithCaseDescription "Skipped because it is not a custom type."

                                    Just _ ->
                                        let
                                            moduleText =
                                                exposeCustomTypeAllTagsInElmModule { customTypeLocalName = localTypeName }
                                                    originalModuleText
                                        in
                                        appFilesWithCaseDescription "Set exposing syntax"
                                            |> Dict.insert moduleFilePath (fileContentFromString moduleText)



{-
   static public string ExposeCustomTypeAllTagsInElmModule(string originalElmModuleText, string customTypeName)
   {
       return AdaptModuleExposeSyntax(originalElmModuleText, originalExposeSyntax =>
       {
           if (originalExposeSyntax.everything != null)
               return originalExposeSyntax;

           var expandedNodes =
               originalExposeSyntax.something
               .Select(originalNode =>
               {
                   if (originalNode.otherTokens != null)
                       return originalNode;

                   if (originalNode.exposeValue.name != customTypeName)
                       return originalNode;

                   if (originalNode.exposeValue.subvalues?.everything != null)
                       return originalNode;

                   return new ModuleExposeSyntax.ModuleExposeSyntaxNode
                   {
                       exposeValue = new ModuleExposeSyntax.ModuleExposeValue
                       {
                           name = originalNode.exposeValue.name,
                           tokensBetweenNameAndDetails = "(",
                           subvalues = new ModuleExposeSyntax.ModuleExposeValueSubvalues { everything = ".." },
                           tokensAfterDetails = ")"
                       }
                   };
               }).ToImmutableList();

           var nodesToAppend =
               expandedNodes.Any(node => node?.exposeValue?.name == customTypeName)
               ?
               new ModuleExposeSyntax.ModuleExposeSyntaxNode[] { }
               :
               new[]{
                   new ModuleExposeSyntax.ModuleExposeSyntaxNode
                   {
                       otherTokens = ", ",
                   },
                   new ModuleExposeSyntax.ModuleExposeSyntaxNode
                   {
                       exposeValue =
                           new ModuleExposeSyntax.ModuleExposeValue
                           {
                               name = customTypeName,
                               tokensBetweenNameAndDetails = "(",
                               subvalues = new ModuleExposeSyntax.ModuleExposeValueSubvalues { everything = ".." },
                               tokensAfterDetails = ")"
                           }
                   }
               };

           return new ModuleExposeSyntax
           {
               something = expandedNodes.AddRange(nodesToAppend),
           };
       });
   }
-}


exposeCustomTypeAllTagsInElmModule : { customTypeLocalName : String } -> String -> String
exposeCustomTypeAllTagsInElmModule { customTypeLocalName } moduleText =
    {-
       adaptModuleExposeSyntax
           (\exposingListNode ->
               let
                   exposingListText =
                       getTextLinesFromRange (Elm.Syntax.Node.range exposingListNode) moduleText
                           |> String.join "\n"
               in
               case Elm.Syntax.Node.value exposingListNode of
                   Elm.Syntax.Exposing.All _ ->
                       exposingListText

                   Elm.Syntax.Exposing.Explicit originalElements ->
                       exposingListText
           )
           moduleText
    -}
    case parseElmModuleText moduleText of
        Err _ ->
            moduleText

        Ok parsedModule ->
            let
                exposingListNode =
                    case Elm.Syntax.Node.value parsedModule.moduleDefinition of
                        Elm.Syntax.Module.NormalModule normalModule ->
                            normalModule.exposingList

                        Elm.Syntax.Module.PortModule portModule ->
                            portModule.exposingList

                        Elm.Syntax.Module.EffectModule effectModule ->
                            effectModule.exposingList

                exposingListRange =
                    Elm.Syntax.Node.range exposingListNode
            in
            case Elm.Syntax.Node.value exposingListNode of
                Elm.Syntax.Exposing.All _ ->
                    moduleText

                Elm.Syntax.Exposing.Explicit _ ->
                    replaceRangeInText exposingListRange "exposing (..)" moduleText



{-
   static string AdaptModuleExposeSyntax(
       string originalElmModuleText,
       Func<ModuleExposeSyntax, ModuleExposeSyntax> adaptExposeSyntax)
   {
       var findAndParseResult = FindAndParseModuleExposeSyntax(originalElmModuleText);

       if (findAndParseResult == null)
           throw new Exception("Failed to find and parse original expose syntax for module:\n" + originalElmModuleText);

       return
           originalElmModuleText.Substring(0, findAndParseResult.Value.beginIndex) +
           adaptExposeSyntax(findAndParseResult.Value.parsedSyntax).ToString() +
           originalElmModuleText.Substring(findAndParseResult.Value.beginIndex + findAndParseResult.Value.parsedLength);
   }
-}


adaptModuleExposeSyntax : (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing -> String) -> String -> String
adaptModuleExposeSyntax updateExposeSyntax moduleText =
    case parseElmModuleText moduleText of
        Err _ ->
            moduleText

        Ok parsedModule ->
            let
                exposingListNode =
                    case Elm.Syntax.Node.value parsedModule.moduleDefinition of
                        Elm.Syntax.Module.NormalModule normalModule ->
                            normalModule.exposingList

                        Elm.Syntax.Module.PortModule portModule ->
                            portModule.exposingList

                        Elm.Syntax.Module.EffectModule effectModule ->
                            effectModule.exposingList

                exposingListRange =
                    Elm.Syntax.Node.range exposingListNode

                exposingListText =
                    getTextLinesFromRange (Elm.Syntax.Node.range exposingListNode) moduleText
            in
            replaceRangeInText
                exposingListRange
                (updateExposeSyntax exposingListNode)
                (moduleText ++ "\n{- Exposing text:\n" ++ String.join "\n" exposingListText ++ "\n-}")


appendLineAndStringInLogFile : String -> AppFiles -> AppFiles
appendLineAndStringInLogFile logLine =
    updateFileContentAtPath
        (\fileContentBefore ->
            [ Maybe.withDefault (fileContentFromString "") fileContentBefore, fileContentFromString ("\n" ++ logLine) ]
                |> List.map Bytes.Encode.bytes
                |> Bytes.Encode.sequence
                |> Bytes.Encode.encode
        )
        [ "compilation-log.txt" ]


updateFileContentAtPath : (Maybe Bytes.Bytes -> Bytes.Bytes) -> List String -> AppFiles -> AppFiles
updateFileContentAtPath updateFileContent filePath appFiles =
    let
        fileContent =
            appFiles
                |> Dict.get filePath
                |> updateFileContent
    in
    appFiles |> Dict.insert filePath fileContent


buildJsonCodingFunctionTexts :
    { typeText : String, encodeExpression : String, decodeExpression : String }
    -> Result String { encodeFunction : { name : String, text : String }, decodeFunction : { name : String, text : String } }
buildJsonCodingFunctionTexts { typeText, encodeExpression, decodeExpression } =
    functionNamesAndTypeParametersFromTypeText typeText
        |> Result.map
            (\{ encodeFunctionName, decodeFunctionName, typeParametersNames } ->
                let
                    annotationsFromTypeList types =
                        types
                            |> List.map (\t -> "(" ++ t ++ ")")
                            |> String.join " -> "

                    inspectionCommentSyntax =
                        -- TODO: Remove inspectionCommentSyntax
                        "\n{- typeText:\n"
                            ++ typeText
                            ++ "\ntypeParametersNames: "
                            ++ (typeParametersNames |> String.join ", ")
                            ++ "-}"

                    typeParameters =
                        {-
                           typeParametersNames
                           .Select(typeParameterName =>
                           {
                               var parameterNameCommonPart = "type_parameter_" + typeParameterName;

                               return
                               new
                               {
                                   encodeAnnotation = typeParameterName + " -> Json.Encode.Value",
                                   encodeParameter = jsonEncodeFunctionNamePrefix + parameterNameCommonPart,

                                   decodeAnnotation = "Json.Decode.Decoder " + typeParameterName,
                                   decodeParameter = jsonDecodeFunctionNamePrefix + parameterNameCommonPart,
                               };
                           })
                        -}
                        typeParametersNames
                            |> List.map
                                (\typeParameterName ->
                                    let
                                        parameterNameCommonPart =
                                            "type_parameter_" ++ typeParameterName
                                    in
                                    { encodeAnnotation = typeParameterName ++ " -> Json.Encode.Value"
                                    , encodeParameter = jsonEncodeFunctionNamePrefix ++ parameterNameCommonPart
                                    , decodeAnnotation = "Json.Decode.Decoder " ++ typeParameterName
                                    , decodeParameter = jsonDecodeFunctionNamePrefix ++ parameterNameCommonPart
                                    }
                                )

                    encodeFunction =
                        encodeFunctionName
                            ++ " : "
                            ++ annotationsFromTypeList
                                ((typeParameters |> List.map .encodeAnnotation) ++ [ typeText, "Json.Encode.Value" ])
                            ++ inspectionCommentSyntax
                            ++ "\n"
                            ++ encodeFunctionName
                            ++ " "
                            ++ (typeParameters |> List.map .encodeParameter |> String.join " ")
                            ++ " "
                            ++ jsonEncodeParamName
                            ++ " =\n"
                            ++ indentElmCodeLines 1 encodeExpression

                    decodeFunction =
                        decodeFunctionName
                            ++ " : "
                            ++ annotationsFromTypeList
                                ((typeParameters |> List.map .decodeAnnotation) ++ [ "Json.Decode.Decoder (" ++ typeText ++ ")" ])
                            ++ inspectionCommentSyntax
                            ++ "\n"
                            ++ decodeFunctionName
                            ++ " "
                            ++ (typeParameters |> List.map .decodeParameter |> String.join " ")
                            ++ " =\n"
                            ++ indentElmCodeLines 1 decodeExpression
                in
                { encodeFunction = { name = encodeFunctionName, text = encodeFunction }
                , decodeFunction = { name = decodeFunctionName, text = decodeFunction }
                }
            )


type alias ResolveTypeResult =
    { canonicalTypeText : String
    , canonicalTypeTextWithParameters : String
    , compileExpressions : () -> Result String { encodeExpression : String, decodeExpression : String, dependencies : Set.Set String }
    , referencedModules : Set.Set String
    }


type alias ExpressionsForType =
    { encodeExpression : String
    , decodeExpression : String
    , referencedModules : Set.Set String
    }


type ElmTypeStructure
    = CustomElmType ElmCustomTypeStructure
    | RecordElmType
        { fields :
            List
                { name : String
                , typeText : String
                , parsedType : ElmTypeStructure
                }
        }
    | AliasElmType
        { aliasLocalName : String
        , parameters : List String
        , aliasedText : String
        }
    | InstanceElmType
        { typeName : String
        , parameters : List String
        }
    | TupleElmType (List String)


type alias ElmCustomTypeStructure =
    { typeLocalName : String
    , parameters : List String
    , tags : Dict.Dict String (List String)
    }


jsonCodingResolveType : { rootTypeText : String, maybeExplicitSourceModuleName : Maybe String, rootSourceModuleText : String } -> Dict.Dict String String -> Result String ResolveTypeResult
jsonCodingResolveType { rootTypeText, maybeExplicitSourceModuleName, rootSourceModuleText } sourceModules =
    let
        sourceModuleText =
            (case maybeExplicitSourceModuleName of
                Just explicitSourceModuleName ->
                    sourceModules |> Dict.get explicitSourceModuleName

                Nothing ->
                    Nothing
            )
                |> Maybe.withDefault rootSourceModuleText

        sourceModuleNamePrefix =
            case maybeExplicitSourceModuleName of
                Nothing ->
                    ""

                Just sourceModuleName ->
                    sourceModuleName ++ "."

        {-
           (string canonicalTypeText, IImmutableSet<string> dependencies) ResolveLocalTypeText(string typeText)
           {
               if (StartsWithLowercaseLetter(typeText))
                   return (typeText, ImmutableHashSet<string>.Empty);

               var canonicalTypeText = ResolveType(typeText, sourceModuleName, sourceModules, logWriteLine).canonicalTypeText;

               return (canonicalTypeText, ImmutableHashSet.Create(canonicalTypeText));
           }
        -}
        resolveLocalTypeText : String -> Result String { canonicalTypeText : String, dependencies : Set.Set String }
        resolveLocalTypeText typeText =
            if stringStartsWithLowercaseLetter typeText then
                Ok { canonicalTypeText = typeText, dependencies = Set.empty }

            else
                jsonCodingResolveType
                    { rootTypeText = typeText
                    , maybeExplicitSourceModuleName = maybeExplicitSourceModuleName
                    , rootSourceModuleText = rootSourceModuleText
                    }
                    sourceModules
                    |> Result.map
                        (\resolved ->
                            { canonicalTypeText = resolved.canonicalTypeText, dependencies = Set.singleton resolved.canonicalTypeText }
                        )

        {-
           ---

           (string encodeExpression, string decodeExpression, IImmutableSet<string> dependencies) compileExpressions()
           {
           var tags = rootType.Custom.Value.tags.Select(typeTag =>
           {
           var typeTagCanonicalName = sourceModuleName + "." + typeTag.Key;

                       string encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax(string argumentSyntax, string objectContentSyntax)
                       {
                           var tagEncodeCase =
                               typeTagCanonicalName + " " + argumentSyntax + " ->";

                           var tagEncodeExpression =
                               @"Json.Encode.object [ ( """ + typeTag.Key + @""", " + objectContentSyntax + " ) ]";

                           return tagEncodeCase + "\n" + IndentElmCodeLines(1, tagEncodeExpression);
                       }

                       var decodeSyntaxCommonPart = @"Json.Decode.field """ + typeTag.Key + @"""";

                       if (typeTag.Value.Count == 0)
                       {
                           return new
                           {
                               encodeCase = encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax("", "Json.Encode.list identity []"),
                               decodeExpression = decodeSyntaxCommonPart + " (Json.Decode.succeed " + typeTagCanonicalName + ")",
                               dependencies = ImmutableHashSet<string>.Empty,
                           };
                       }

                       var tagParametersExpressions =
                           typeTag.Value.Select(tagParameterType =>
                           {
                               var tagParameterCanonicalTypeTextAndDependencies = ResolveLocalTypeText(tagParameterType);

                               var (tagParameterTypeFunctionNames, tagParameterTypeParametersNames) =
                                   GetFunctionNamesAndTypeParametersFromTypeText(tagParameterCanonicalTypeTextAndDependencies.canonicalTypeText);

                               var tagParameterEncodeFunction =
                                   expressionTextForFunctionWithOptionalTypeParameters(
                                       tagParameterTypeFunctionNames.encodeFunctionName, jsonEncodeFunctionNamePrefix, tagParameterTypeParametersNames);

                               var tagParameterDecodeFunction =
                                   expressionTextForFunctionWithOptionalTypeParameters(
                                       tagParameterTypeFunctionNames.decodeFunctionName, jsonDecodeFunctionNamePrefix, tagParameterTypeParametersNames);

                               return new
                               {
                                   encodeFunction = tagParameterEncodeFunction,
                                   decodeFunction = tagParameterDecodeFunction,
                                   canonicalTypeTextAndDependencies = tagParameterCanonicalTypeTextAndDependencies,
                               };
                           }).ToImmutableList();

                       var tagDecodeExpressionBeforeLazy =
                           tagParametersExpressions.Count == 1
                           /*
                           2020-03-07: To support easy migration of existing applications, support decoding from the older JSON representation for tags with one parameter.
                           This branch can be removed when all apps have been migrated.
                           */
                           ?
                           "Json.Decode.map " + typeTagCanonicalName +
                           " (Json.Decode.oneOf [ " + tagParametersExpressions.Single().decodeFunction + ", " +
                           "(Json.Decode.index 0 " + tagParametersExpressions.Single().decodeFunction + ")" +
                           " ])"
                           :
                           "Json.Decode.map" + tagParametersExpressions.Count.ToString() + " " + typeTagCanonicalName + " " +
                               string.Join(" ", tagParametersExpressions.Select((tagParamExpr, tagParamIndex) =>
                                   "(Json.Decode.index " + tagParamIndex.ToString() + " " + tagParamExpr.decodeFunction + ")"));

                       var tagDecodeExpression =
                           decodeSyntaxCommonPart + @" (Json.Decode.lazy (\_ -> " +
                           tagDecodeExpressionBeforeLazy +
                           " ) )";

                       var encodeArgumentsAndExpressions =
                           tagParametersExpressions
                           .Select((tagArgumentExpressions, tagParameterIndex) =>
                           {
                               var argumentName = "tagArgument" + tagParameterIndex.ToString();

                               return (argumentName, encodeExpression: argumentName + " |> " + tagArgumentExpressions.encodeFunction);
                           }).ToImmutableList();

                       var encodeArgumentSyntax =
                           string.Join(" ", encodeArgumentsAndExpressions.Select(argumentAndExpr => argumentAndExpr.argumentName));

                       var encodeObjectContentSyntax =
                           "[ " +
                           string.Join(", ", encodeArgumentsAndExpressions.Select(argumentAndExpr => argumentAndExpr.encodeExpression))
                           + " ] |> Json.Encode.list identity";

                       var tagParametersDependencies =
                           tagParametersExpressions
                           .Select(tagParamResult => tagParamResult.canonicalTypeTextAndDependencies.dependencies)
                           .Aggregate(ImmutableHashSet<string>.Empty, (a, b) => a.Union(b));

                       return new
                       {
                           encodeCase = encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax(
                               encodeArgumentSyntax,
                               encodeObjectContentSyntax),
                           decodeExpression = tagDecodeExpression,
                           dependencies = tagParametersDependencies,
                       };
                   })
                   .ToImmutableList();

               var encodeCases =
                   String.Join("\n\n", tags.Select(tag => tag.encodeCase));

               var encodeExpression =
                   "case " + encodeParamName + " of\n" +
                   IndentElmCodeLines(1, encodeCases);

               var decodeArrayExpression = "[ " + String.Join("\n, ", tags.Select(tag => tag.decodeExpression)) + "\n]";

               var decodeExpression =
                   "Json.Decode.oneOf\n" +
                   IndentElmCodeLines(1, decodeArrayExpression);

               var allTagsDependencies =
                   tags.SelectMany(field => field.dependencies)
                   .ToImmutableHashSet();

               return (encodeExpression, decodeExpression, allTagsDependencies);

           }

           var canonicalTypeText = sourceModuleName + "." + rootType.Custom.Value.typeLocalName;

           var canonicalTypeTextWithParameters =
           rootType.Custom.Value.parameters.Count < 1
           ?
           canonicalTypeText
           :
           "(" + canonicalTypeText + " " + string.Join(" ", rootType.Custom.Value.parameters) + ")";

           return new ResolveTypeResult
           {
           canonicalTypeText = canonicalTypeText,
           canonicalTypeTextWithParameters = canonicalTypeTextWithParameters,

               compileExpressions = compileExpressions,

               referencedModules = ImmutableHashSet<string>.Empty.Add(sourceModuleName),

           };

        -}
        jsonCodingResolveTypeCustomType : ElmCustomTypeStructure -> Result String ResolveTypeResult
        jsonCodingResolveTypeCustomType customType =
            let
                compileExpressions : () -> Result String { encodeExpression : String, decodeExpression : String, dependencies : Set.Set String }
                compileExpressions _ =
                    {-
                            var tags = rootType.Custom.Value.tags.Select(typeTag =>
                       {
                       var typeTagCanonicalName = sourceModuleName + "." + typeTag.Key;

                                   string encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax(string argumentSyntax, string objectContentSyntax)
                                   {
                                       var tagEncodeCase =
                                           typeTagCanonicalName + " " + argumentSyntax + " ->";

                                       var tagEncodeExpression =
                                           @"Json.Encode.object [ ( """ + typeTag.Key + @""", " + objectContentSyntax + " ) ]";

                                       return tagEncodeCase + "\n" + IndentElmCodeLines(1, tagEncodeExpression);
                                   }

                                   var decodeSyntaxCommonPart = @"Json.Decode.field """ + typeTag.Key + @"""";

                                   if (typeTag.Value.Count == 0)
                                   {
                                       return new
                                       {
                                           encodeCase = encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax("", "Json.Encode.list identity []"),
                                           decodeExpression = decodeSyntaxCommonPart + " (Json.Decode.succeed " + typeTagCanonicalName + ")",
                                           dependencies = ImmutableHashSet<string>.Empty,
                                       };
                                   }

                                   var tagParametersExpressions =
                                       typeTag.Value.Select(tagParameterType =>
                                       {
                                           var tagParameterCanonicalTypeTextAndDependencies = ResolveLocalTypeText(tagParameterType);

                                           var (tagParameterTypeFunctionNames, tagParameterTypeParametersNames) =
                                               GetFunctionNamesAndTypeParametersFromTypeText(tagParameterCanonicalTypeTextAndDependencies.canonicalTypeText);

                                           var tagParameterEncodeFunction =
                                               expressionTextForFunctionWithOptionalTypeParameters(
                                                   tagParameterTypeFunctionNames.encodeFunctionName, jsonEncodeFunctionNamePrefix, tagParameterTypeParametersNames);

                                           var tagParameterDecodeFunction =
                                               expressionTextForFunctionWithOptionalTypeParameters(
                                                   tagParameterTypeFunctionNames.decodeFunctionName, jsonDecodeFunctionNamePrefix, tagParameterTypeParametersNames);

                                           return new
                                           {
                                               encodeFunction = tagParameterEncodeFunction,
                                               decodeFunction = tagParameterDecodeFunction,
                                               canonicalTypeTextAndDependencies = tagParameterCanonicalTypeTextAndDependencies,
                                           };
                                       }).ToImmutableList();

                                   var tagDecodeExpressionBeforeLazy =
                                       tagParametersExpressions.Count == 1
                                       /*
                                       2020-03-07: To support easy migration of existing applications, support decoding from the older JSON representation for tags with one parameter.
                                       This branch can be removed when all apps have been migrated.
                                       */
                                       ?
                                       "Json.Decode.map " + typeTagCanonicalName +
                                       " (Json.Decode.oneOf [ " + tagParametersExpressions.Single().decodeFunction + ", " +
                                       "(Json.Decode.index 0 " + tagParametersExpressions.Single().decodeFunction + ")" +
                                       " ])"
                                       :
                                       "Json.Decode.map" + tagParametersExpressions.Count.ToString() + " " + typeTagCanonicalName + " " +
                                           string.Join(" ", tagParametersExpressions.Select((tagParamExpr, tagParamIndex) =>
                                               "(Json.Decode.index " + tagParamIndex.ToString() + " " + tagParamExpr.decodeFunction + ")"));

                                   var tagDecodeExpression =
                                       decodeSyntaxCommonPart + @" (Json.Decode.lazy (\_ -> " +
                                       tagDecodeExpressionBeforeLazy +
                                       " ) )";

                                   var encodeArgumentsAndExpressions =
                                       tagParametersExpressions
                                       .Select((tagArgumentExpressions, tagParameterIndex) =>
                                       {
                                           var argumentName = "tagArgument" + tagParameterIndex.ToString();

                                           return (argumentName, encodeExpression: argumentName + " |> " + tagArgumentExpressions.encodeFunction);
                                       }).ToImmutableList();

                                   var encodeArgumentSyntax =
                                       string.Join(" ", encodeArgumentsAndExpressions.Select(argumentAndExpr => argumentAndExpr.argumentName));

                                   var encodeObjectContentSyntax =
                                       "[ " +
                                       string.Join(", ", encodeArgumentsAndExpressions.Select(argumentAndExpr => argumentAndExpr.encodeExpression))
                                       + " ] |> Json.Encode.list identity";

                                   var tagParametersDependencies =
                                       tagParametersExpressions
                                       .Select(tagParamResult => tagParamResult.canonicalTypeTextAndDependencies.dependencies)
                                       .Aggregate(ImmutableHashSet<string>.Empty, (a, b) => a.Union(b));

                                   return new
                                   {
                                       encodeCase = encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax(
                                           encodeArgumentSyntax,
                                           encodeObjectContentSyntax),
                                       decodeExpression = tagDecodeExpression,
                                       dependencies = tagParametersDependencies,
                                   };
                               })
                               .ToImmutableList();
                    -}
                    let
                        tagsResults =
                            customType.tags
                                |> Dict.toList
                                |> List.map
                                    (\( tagName, tagParameters ) ->
                                        let
                                            typeTagCanonicalName =
                                                sourceModuleNamePrefix ++ tagName

                                            encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax argumentSyntax objectContentSyntax =
                                                let
                                                    tagEncodeCase =
                                                        typeTagCanonicalName ++ " " ++ argumentSyntax ++ " ->"

                                                    tagEncodeExpression =
                                                        "Json.Encode.object [ ( \"" ++ tagName ++ "\", " ++ objectContentSyntax ++ " ) ]"
                                                in
                                                tagEncodeCase ++ "\n" ++ indentElmCodeLines 1 tagEncodeExpression

                                            decodeSyntaxCommonPart =
                                                "Json.Decode.field \"" ++ tagName ++ "\""
                                        in
                                        if tagParameters == [] then
                                            Ok
                                                { encodeCase = encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax "" "Json.Encode.list identity []"
                                                , decodeExpression = decodeSyntaxCommonPart ++ " (Json.Decode.succeed " ++ typeTagCanonicalName ++ ")"
                                                , dependencies = Set.empty
                                                }

                                        else
                                            {-
                                               var tagParametersExpressions =
                                                      typeTag.Value.Select(tagParameterType =>
                                                      {
                                                          var tagParameterCanonicalTypeTextAndDependencies = ResolveLocalTypeText(tagParameterType);

                                                          var (tagParameterTypeFunctionNames, tagParameterTypeParametersNames) =
                                                              GetFunctionNamesAndTypeParametersFromTypeText(tagParameterCanonicalTypeTextAndDependencies.canonicalTypeText);

                                                          var tagParameterEncodeFunction =
                                                              expressionTextForFunctionWithOptionalTypeParameters(
                                                                  tagParameterTypeFunctionNames.encodeFunctionName, jsonEncodeFunctionNamePrefix, tagParameterTypeParametersNames);

                                                          var tagParameterDecodeFunction =
                                                              expressionTextForFunctionWithOptionalTypeParameters(
                                                                  tagParameterTypeFunctionNames.decodeFunctionName, jsonDecodeFunctionNamePrefix, tagParameterTypeParametersNames);

                                                          return new
                                                          {
                                                              encodeFunction = tagParameterEncodeFunction,
                                                              decodeFunction = tagParameterDecodeFunction,
                                                              canonicalTypeTextAndDependencies = tagParameterCanonicalTypeTextAndDependencies,
                                                          };
                                                      }).ToImmutableList();

                                                  var tagDecodeExpressionBeforeLazy =
                                                      tagParametersExpressions.Count == 1
                                                      /*
                                                      2020-03-07: To support easy migration of existing applications, support decoding from the older JSON representation for tags with one parameter.
                                                      This branch can be removed when all apps have been migrated.
                                                      */
                                                      ?
                                                      "Json.Decode.map " + typeTagCanonicalName +
                                                      " (Json.Decode.oneOf [ " + tagParametersExpressions.Single().decodeFunction + ", " +
                                                      "(Json.Decode.index 0 " + tagParametersExpressions.Single().decodeFunction + ")" +
                                                      " ])"
                                                      :
                                                      "Json.Decode.map" + tagParametersExpressions.Count.ToString() + " " + typeTagCanonicalName + " " +
                                                          string.Join(" ", tagParametersExpressions.Select((tagParamExpr, tagParamIndex) =>
                                                              "(Json.Decode.index " + tagParamIndex.ToString() + " " + tagParamExpr.decodeFunction + ")"));

                                                  var tagDecodeExpression =
                                                      decodeSyntaxCommonPart + @" (Json.Decode.lazy (\_ -> " +
                                                      tagDecodeExpressionBeforeLazy +
                                                      " ) )";

                                                  var encodeArgumentsAndExpressions =
                                                      tagParametersExpressions
                                                      .Select((tagArgumentExpressions, tagParameterIndex) =>
                                                      {
                                                          var argumentName = "tagArgument" + tagParameterIndex.ToString();

                                                          return (argumentName, encodeExpression: argumentName + " |> " + tagArgumentExpressions.encodeFunction);
                                                      }).ToImmutableList();

                                                  var encodeArgumentSyntax =
                                                      string.Join(" ", encodeArgumentsAndExpressions.Select(argumentAndExpr => argumentAndExpr.argumentName));

                                                  var encodeObjectContentSyntax =
                                                      "[ " +
                                                      string.Join(", ", encodeArgumentsAndExpressions.Select(argumentAndExpr => argumentAndExpr.encodeExpression))
                                                      + " ] |> Json.Encode.list identity";

                                                  var tagParametersDependencies =
                                                      tagParametersExpressions
                                                      .Select(tagParamResult => tagParamResult.canonicalTypeTextAndDependencies.dependencies)
                                                      .Aggregate(ImmutableHashSet<string>.Empty, (a, b) => a.Union(b));

                                                  return new
                                                  {
                                                      encodeCase = encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax(
                                                          encodeArgumentSyntax,
                                                          encodeObjectContentSyntax),
                                                      decodeExpression = tagDecodeExpression,
                                                      dependencies = tagParametersDependencies,
                                                  };
                                            -}
                                            let
                                                tagParametersExpressionsResults =
                                                    tagParameters
                                                        |> List.map
                                                            (\tagParameterType ->
                                                                resolveLocalTypeText tagParameterType
                                                                    |> Result.andThen
                                                                        (\tagParameterTypeResolution ->
                                                                            functionNamesAndTypeParametersFromTypeText tagParameterTypeResolution.canonicalTypeText
                                                                                |> Result.map
                                                                                    (\tagParameterTypeFunctionNames ->
                                                                                        let
                                                                                            tagParameterEncodeFunction =
                                                                                                expressionTextForFunctionWithOptionalTypeParameters
                                                                                                    { functionName = tagParameterTypeFunctionNames.encodeFunctionName
                                                                                                    , typeParametersFunctionsCommonPrefix = jsonEncodeFunctionNamePrefix
                                                                                                    , typeParametersNames = tagParameterTypeFunctionNames.typeParametersNames
                                                                                                    }

                                                                                            tagParameterDecodeFunction =
                                                                                                expressionTextForFunctionWithOptionalTypeParameters
                                                                                                    { functionName = tagParameterTypeFunctionNames.decodeFunctionName
                                                                                                    , typeParametersFunctionsCommonPrefix = jsonDecodeFunctionNamePrefix
                                                                                                    , typeParametersNames = tagParameterTypeFunctionNames.typeParametersNames
                                                                                                    }
                                                                                        in
                                                                                        { encodeFunction = tagParameterEncodeFunction
                                                                                        , decodeFunction = tagParameterDecodeFunction
                                                                                        , canonicalTypeTextAndDependencies = tagParameterTypeResolution
                                                                                        }
                                                                                    )
                                                                        )
                                                            )
                                            in
                                            case Result.Extra.combine tagParametersExpressionsResults of
                                                Err error ->
                                                    Err ("Failed to build tag parameter expression: " ++ error)

                                                Ok tagParametersExpressions ->
                                                    let
                                                        tagDecodeExpressionBeforeLazy =
                                                            "Json.Decode.map"
                                                                ++ (if List.length tagParametersExpressions == 1 then
                                                                        ""

                                                                    else
                                                                        String.fromInt (List.length tagParametersExpressions)
                                                                   )
                                                                ++ " "
                                                                ++ typeTagCanonicalName
                                                                ++ " "
                                                                ++ String.join " "
                                                                    (tagParametersExpressions
                                                                        |> List.indexedMap
                                                                            (\tagParamIndex tagParamExpr ->
                                                                                "(Json.Decode.index " ++ String.fromInt tagParamIndex ++ " " ++ tagParamExpr.decodeFunction ++ ")"
                                                                            )
                                                                    )

                                                        tagDecodeExpression =
                                                            decodeSyntaxCommonPart
                                                                ++ " (Json.Decode.lazy (\\_ -> "
                                                                ++ tagDecodeExpressionBeforeLazy
                                                                ++ " ) )"

                                                        encodeArgumentsAndExpressions =
                                                            tagParametersExpressions
                                                                |> List.indexedMap
                                                                    (\tagParameterIndex tagArgumentExpressions ->
                                                                        let
                                                                            argumentName =
                                                                                "tagArgument" ++ String.fromInt tagParameterIndex
                                                                        in
                                                                        { argumentName = argumentName
                                                                        , encodeExpression = argumentName ++ " |> " ++ tagArgumentExpressions.encodeFunction
                                                                        }
                                                                    )

                                                        encodeArgumentSyntax =
                                                            String.join " " (encodeArgumentsAndExpressions |> List.map .argumentName)

                                                        encodeObjectContentSyntax =
                                                            "[ "
                                                                ++ String.join ", " (encodeArgumentsAndExpressions |> List.map .encodeExpression)
                                                                ++ " ] |> Json.Encode.list identity"

                                                        tagParametersDependencies =
                                                            tagParametersExpressions
                                                                |> List.map (.canonicalTypeTextAndDependencies >> .dependencies)
                                                                |> List.foldl Set.union Set.empty
                                                    in
                                                    Ok
                                                        { encodeCase =
                                                            encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax
                                                                encodeArgumentSyntax
                                                                encodeObjectContentSyntax
                                                        , decodeExpression = tagDecodeExpression
                                                        , dependencies = tagParametersDependencies
                                                        }
                                    )
                    in
                    case Result.Extra.combine tagsResults of
                        Err error ->
                            Err ("Failed for tags: " ++ error)

                        Ok tags ->
                            {-
                               var encodeCases =
                                   String.Join("\n\n", tags.Select(tag => tag.encodeCase));

                               var encodeExpression =
                                   "case " + encodeParamName + " of\n" +
                                   IndentElmCodeLines(1, encodeCases);

                               var decodeArrayExpression = "[ " + String.Join("\n, ", tags.Select(tag => tag.decodeExpression)) + "\n]";

                               var decodeExpression =
                                   "Json.Decode.oneOf\n" +
                                   IndentElmCodeLines(1, decodeArrayExpression);

                               var allTagsDependencies =
                                   tags.SelectMany(field => field.dependencies)
                                   .ToImmutableHashSet();

                               return (encodeExpression, decodeExpression, allTaagsDependencies);
                            -}
                            let
                                encodeCases =
                                    String.join "\n\n" (tags |> List.map .encodeCase)

                                encodeExpression =
                                    "case "
                                        ++ jsonEncodeParamName
                                        ++ " of\n"
                                        ++ indentElmCodeLines 1 encodeCases

                                decodeArrayExpression =
                                    "[ " ++ String.join "\n, " (tags |> List.map .decodeExpression) ++ "\n]"

                                decodeExpression =
                                    "Json.Decode.oneOf\n"
                                        ++ indentElmCodeLines 1 decodeArrayExpression

                                allTagsDependencies =
                                    tags
                                        |> List.map .dependencies
                                        |> List.foldl Set.union Set.empty
                            in
                            Ok
                                { encodeExpression = encodeExpression
                                , decodeExpression = decodeExpression
                                , dependencies = allTagsDependencies
                                }

                canonicalTypeText =
                    sourceModuleNamePrefix ++ customType.typeLocalName

                canonicalTypeTextWithParameters =
                    if List.length customType.parameters < 1 then
                        canonicalTypeText

                    else
                        "(" ++ canonicalTypeText ++ " " ++ String.join " " customType.parameters ++ ")"
            in
            Ok
                { canonicalTypeText = canonicalTypeText
                , canonicalTypeTextWithParameters = canonicalTypeTextWithParameters
                , compileExpressions = compileExpressions
                , referencedModules = maybeExplicitSourceModuleName |> Maybe.map Set.singleton |> Maybe.withDefault Set.empty
                }
    in
    parseAndMapElmModuleText
        (\parsedSourceModule ->
            let
                getCanonicalModuleNameAndLocalTypeNameFromNameInSourceModule : String -> Result String ( List String, String )
                getCanonicalModuleNameAndLocalTypeNameFromNameInSourceModule nameInThisModule =
                    case getCanonicalNameFromImportedNameInModule parsedSourceModule nameInThisModule of
                        Just importedName ->
                            Ok importedName

                        Nothing ->
                            case maybeExplicitSourceModuleName of
                                Just explicitSourceModuleName ->
                                    Ok ( String.split "." explicitSourceModuleName, nameInThisModule )

                                Nothing ->
                                    Err ("Failed to look up name '" ++ nameInThisModule ++ "'.")
            in
            case Dict.get rootTypeText jsonCodeLeafExpressions of
                Just ( encodeExpression, decodeExpression ) ->
                    Ok
                        { canonicalTypeText = rootTypeText
                        , canonicalTypeTextWithParameters = rootTypeText
                        , compileExpressions =
                            \() ->
                                Ok
                                    { encodeExpression = encodeExpression
                                    , decodeExpression = decodeExpression
                                    , dependencies = Set.empty
                                    }
                        , referencedModules = Set.empty
                        }

                Nothing ->
                    case parseElmTypeText True rootTypeText of
                        Err error ->
                            Err ("Failed to parse Elm type: " ++ error ++ ", type text: '" ++ rootTypeText ++ "'")

                        Ok ( rootType, _ ) ->
                            case rootType of
                                InstanceElmType instanceElmType ->
                                    if instanceElmType.parameters == [] then
                                        {-
                                           var (referencedModuleName, typeNameInReferencedModule) =
                                               GetCanonicalModuleNameAndLocalTypeNameFromNameInSourceModule(rootTypeText);

                                           if (referencedModuleName != sourceModuleName)
                                           {
                                               logWriteLine?.Invoke("Type '" + rootTypeText + "' in '" + sourceModuleName + "' refers to '" + referencedModuleName + "." + typeNameInReferencedModule + "'.");

                                               return ResolveType(
                                                   typeNameInReferencedModule,
                                                   referencedModuleName,
                                                   sourceModules,
                                                   logWriteLine);
                                           }

                                           logWriteLine?.Invoke("Resolve type text for '" + rootTypeText + "' in '" + sourceModuleName + "'.");

                                           var referencedTypeText = GetTypeDefinitionTextFromModuleText(rootTypeText, sourceModuleText);

                                           if (!(0 < referencedTypeText?.Length))
                                               throw new Exception("Did not find the definition of type '" + rootTypeText + "'.");

                                           return ResolveType(referencedTypeText, sourceModuleName, sourceModules, logWriteLine);
                                        -}
                                        case getCanonicalNameFromImportedNameInModule parsedSourceModule rootTypeText of
                                            Nothing ->
                                                case getTypeDefinitionTextFromModuleText instanceElmType.typeName sourceModuleText of
                                                    Err error ->
                                                        Err
                                                            ("Failed to get type definition text for local name '"
                                                                ++ instanceElmType.typeName
                                                                ++ "' ("
                                                                ++ rootTypeText
                                                                ++ ") in module '"
                                                                ++ Maybe.withDefault "" maybeExplicitSourceModuleName
                                                                ++ "': "
                                                                ++ error
                                                            )

                                                    Ok Nothing ->
                                                        Err
                                                            ("Failed to get type definition text for local name '"
                                                                ++ instanceElmType.typeName
                                                                ++ "' ("
                                                                ++ rootTypeText
                                                                ++ ") in module '"
                                                                ++ Maybe.withDefault "" maybeExplicitSourceModuleName
                                                                ++ "'"
                                                            )

                                                    Ok (Just typeDefinitionText) ->
                                                        jsonCodingResolveType
                                                            { rootTypeText = typeDefinitionText
                                                            , maybeExplicitSourceModuleName = maybeExplicitSourceModuleName
                                                            , rootSourceModuleText = rootSourceModuleText
                                                            }
                                                            sourceModules

                                            Just ( originatingModuleName, nameInImportedModule ) ->
                                                jsonCodingResolveType
                                                    { rootTypeText = nameInImportedModule
                                                    , maybeExplicitSourceModuleName = Just (String.join "." originatingModuleName)
                                                    , rootSourceModuleText = rootSourceModuleText
                                                    }
                                                    sourceModules

                                    else
                                        {-
                                                               var parameters =
                                               rootType.Instance.Value.parameters
                                               .Select(parameter =>
                                               {
                                                   var dependencies = ResolveType(parameter, sourceModuleName, sourceModules, logWriteLine);

                                                   var (functionNames, typeParametersNames) = GetFunctionNamesAndTypeParametersFromTypeText(dependencies.canonicalTypeText);

                                                   var encodeFunction = expressionTextForFunctionWithOptionalTypeParameters(
                                                       functionNames.encodeFunctionName, jsonEncodeFunctionNamePrefix, typeParametersNames);

                                                   var decodeFunction = expressionTextForFunctionWithOptionalTypeParameters(
                                                       functionNames.decodeFunctionName, jsonDecodeFunctionNamePrefix, typeParametersNames);

                                                   return new { encodeFunction, decodeFunction, dependencies };
                                               })
                                               .ToImmutableList();

                                           string instantiatedTypeCanonicalName = null;
                                           string instantiatedTypeFunctionNameCommonPart = null;
                                           IImmutableSet<string> dependenciesFromInstantiatedType = ImmutableHashSet<string>.Empty;

                                           if (InstantiationSpecialCases.TryGetValue(rootType.Instance.Value.typeName, out var specialCaseFunctionNameCommonPart))
                                           {
                                               instantiatedTypeCanonicalName = rootType.Instance.Value.typeName;
                                               instantiatedTypeFunctionNameCommonPart = specialCaseFunctionNameCommonPart;
                                           }
                                           else
                                           {
                                               var instantiatedTypeResolution = ResolveType(rootType.Instance.Value.typeName, sourceModuleName, sourceModules, logWriteLine);
                                               instantiatedTypeCanonicalName = instantiatedTypeResolution.canonicalTypeText;
                                               instantiatedTypeFunctionNameCommonPart = GetFunctionNamesAndTypeParametersFromTypeText(instantiatedTypeCanonicalName).functionNames.commonPart;
                                               dependenciesFromInstantiatedType = ImmutableHashSet.Create(instantiatedTypeResolution.canonicalTypeText);
                                           }

                                           var parametersTypeTextsForComposition =
                                               parameters.Select(param =>
                                               {
                                                   var needsParentheses =
                                                       param.dependencies.canonicalTypeText.Contains(" ") &&
                                                       !param.dependencies.canonicalTypeText.StartsWith("(") &&
                                                       !param.dependencies.canonicalTypeText.StartsWith("{");

                                                   var beforeParentheses =
                                                       param.dependencies.canonicalTypeText;

                                                   if (needsParentheses)
                                                       return "(" + beforeParentheses + ")";

                                                   return beforeParentheses;
                                               });

                                           return new ResolveTypeResult
                                           {
                                               canonicalTypeText = instantiatedTypeCanonicalName + " " + String.Join(" ", parametersTypeTextsForComposition),

                                               compileExpressions = () =>
                                                   (jsonEncodeFunctionNamePrefix + instantiatedTypeFunctionNameCommonPart + " " + String.Join(" ", parameters.Select(param => param.encodeFunction)) + " " + encodeParamName,
                                                   jsonDecodeFunctionNamePrefix + instantiatedTypeFunctionNameCommonPart + " " + String.Join(" ", parameters.Select(param => param.decodeFunction)),
                                                   dependenciesFromInstantiatedType.Union(parameters.Select(parameter => parameter.dependencies.canonicalTypeText))),

                                               referencedModules = ImmutableHashSet<string>.Empty,
                                           };
                                        -}
                                        case
                                            instanceElmType.parameters
                                                |> List.map
                                                    (\parameter ->
                                                        jsonCodingResolveType
                                                            { rootTypeText = parameter
                                                            , maybeExplicitSourceModuleName = maybeExplicitSourceModuleName
                                                            , rootSourceModuleText = rootSourceModuleText
                                                            }
                                                            sourceModules
                                                            |> Result.andThen
                                                                (\paramDependencies ->
                                                                    {-
                                                                       var (functionNames, typeParametersNames) = GetFunctionNamesAndTypeParametersFromTypeText(dependencies.canonicalTypeText);

                                                                       var encodeFunction = expressionTextForFunctionWithOptionalTypeParameters(
                                                                           functionNames.encodeFunctionName, jsonEncodeFunctionNamePrefix, typeParametersNames);

                                                                       var decodeFunction = expressionTextForFunctionWithOptionalTypeParameters(
                                                                           functionNames.decodeFunctionName, jsonDecodeFunctionNamePrefix, typeParametersNames);

                                                                       return new { encodeFunction, decodeFunction, dependencies };
                                                                    -}
                                                                    functionNamesAndTypeParametersFromTypeText paramDependencies.canonicalTypeText
                                                                        |> Result.map
                                                                            (\paramFunctionNames ->
                                                                                let
                                                                                    encodeFunction =
                                                                                        expressionTextForFunctionWithOptionalTypeParameters
                                                                                            { functionName = paramFunctionNames.encodeFunctionName
                                                                                            , typeParametersFunctionsCommonPrefix = jsonEncodeFunctionNamePrefix
                                                                                            , typeParametersNames = paramFunctionNames.typeParametersNames
                                                                                            }

                                                                                    decodeFunction =
                                                                                        expressionTextForFunctionWithOptionalTypeParameters
                                                                                            { functionName = paramFunctionNames.decodeFunctionName
                                                                                            , typeParametersFunctionsCommonPrefix = jsonDecodeFunctionNamePrefix
                                                                                            , typeParametersNames = paramFunctionNames.typeParametersNames
                                                                                            }
                                                                                in
                                                                                { encodeFunction = encodeFunction
                                                                                , decodeFunction = decodeFunction
                                                                                , dependencies = paramDependencies
                                                                                }
                                                                            )
                                                                )
                                                    )
                                                |> Result.Extra.combine
                                        of
                                            Err error ->
                                                Err ("Failed to map instance parameter: " ++ error)

                                            Ok parameters ->
                                                {-
                                                   string instantiatedTypeCanonicalName = null;
                                                      string instantiatedTypeFunctionNameCommonPart = null;
                                                      IImmutableSet<string> dependenciesFromInstantiatedType = ImmutableHashSet<string>.Empty;

                                                      if (InstantiationSpecialCases.TryGetValue(rootType.Instance.Value.typeName, out var specialCaseFunctionNameCommonPart))
                                                      {
                                                          instantiatedTypeCanonicalName = rootType.Instance.Value.typeName;
                                                          instantiatedTypeFunctionNameCommonPart = specialCaseFunctionNameCommonPart;
                                                      }
                                                      else
                                                      {
                                                          var instantiatedTypeResolution = ResolveType(rootType.Instance.Value.typeName, sourceModuleName, sourceModules, logWriteLine);
                                                          instantiatedTypeCanonicalName = instantiatedTypeResolution.canonicalTypeText;
                                                          instantiatedTypeFunctionNameCommonPart = GetFunctionNamesAndTypeParametersFromTypeText(instantiatedTypeCanonicalName).functionNames.commonPart;
                                                          dependenciesFromInstantiatedType = ImmutableHashSet.Create(instantiatedTypeResolution.canonicalTypeText);
                                                      }

                                                      var parametersTypeTextsForComposition =
                                                          parameters.Select(param =>
                                                          {
                                                              var needsParentheses =
                                                                  param.dependencies.canonicalTypeText.Contains(" ") &&
                                                                  !param.dependencies.canonicalTypeText.StartsWith("(") &&
                                                                  !param.dependencies.canonicalTypeText.StartsWith("{");

                                                              var beforeParentheses =
                                                                  param.dependencies.canonicalTypeText;

                                                              if (needsParentheses)
                                                                  return "(" + beforeParentheses + ")";

                                                              return beforeParentheses;
                                                          });

                                                      return new ResolveTypeResult
                                                      {
                                                          canonicalTypeText = instantiatedTypeCanonicalName + " " + String.Join(" ", parametersTypeTextsForComposition),

                                                          compileExpressions = () =>
                                                              (jsonEncodeFunctionNamePrefix + instantiatedTypeFunctionNameCommonPart + " " + String.Join(" ", parameters.Select(param => param.encodeFunction)) + " " + encodeParamName,
                                                              jsonDecodeFunctionNamePrefix + instantiatedTypeFunctionNameCommonPart + " " + String.Join(" ", parameters.Select(param => param.decodeFunction)),
                                                              dependenciesFromInstantiatedType.Union(parameters.Select(parameter => parameter.dependencies.canonicalTypeText))),

                                                          referencedModules = ImmutableHashSet<string>.Empty,
                                                      };
                                                -}
                                                let
                                                    continueWithComponents components =
                                                        let
                                                            parametersTypeTextsForComposition =
                                                                parameters
                                                                    |> List.map
                                                                        (\param ->
                                                                            let
                                                                                needsParentheses =
                                                                                    String.contains " " param.dependencies.canonicalTypeText
                                                                                        && not (param.dependencies.canonicalTypeText |> String.startsWith "(")
                                                                                        && not (param.dependencies.canonicalTypeText |> String.startsWith "{")

                                                                                beforeParentheses =
                                                                                    param.dependencies.canonicalTypeText
                                                                            in
                                                                            if needsParentheses then
                                                                                "(" ++ beforeParentheses ++ ")"

                                                                            else
                                                                                beforeParentheses
                                                                        )

                                                            canonicalTypeText =
                                                                components.instantiatedTypeCanonicalName ++ " " ++ String.join " " parametersTypeTextsForComposition
                                                        in
                                                        { canonicalTypeText = canonicalTypeText
                                                        , canonicalTypeTextWithParameters = canonicalTypeText
                                                        , compileExpressions =
                                                            \() ->
                                                                Ok
                                                                    { encodeExpression =
                                                                        jsonEncodeFunctionNamePrefix ++ components.instantiatedTypeFunctionNameCommonPart ++ " " ++ String.join " " (parameters |> List.map .encodeFunction) ++ " " ++ jsonEncodeParamName
                                                                    , decodeExpression =
                                                                        jsonDecodeFunctionNamePrefix ++ components.instantiatedTypeFunctionNameCommonPart ++ " " ++ String.join " " (parameters |> List.map .decodeFunction)
                                                                    , dependencies =
                                                                        components.dependenciesFromInstantiatedType
                                                                            :: (parameters |> List.map (.dependencies >> .canonicalTypeText >> Set.singleton))
                                                                            |> List.foldl Set.union Set.empty
                                                                    }
                                                        , referencedModules = maybeExplicitSourceModuleName |> Maybe.map Set.singleton |> Maybe.withDefault Set.empty
                                                        }
                                                in
                                                case instantiationSpecialCases |> Dict.get instanceElmType.typeName of
                                                    Just instantiatedTypeFunctionNameCommonPart ->
                                                        Ok
                                                            (continueWithComponents
                                                                { instantiatedTypeCanonicalName = instanceElmType.typeName
                                                                , instantiatedTypeFunctionNameCommonPart = instantiatedTypeFunctionNameCommonPart
                                                                , dependenciesFromInstantiatedType = Set.empty
                                                                }
                                                            )

                                                    Nothing ->
                                                        case
                                                            jsonCodingResolveType
                                                                { rootTypeText = instanceElmType.typeName
                                                                , maybeExplicitSourceModuleName = maybeExplicitSourceModuleName
                                                                , rootSourceModuleText = rootSourceModuleText
                                                                }
                                                                sourceModules
                                                        of
                                                            Err error ->
                                                                Err ("Failed to resolve instantiated type '" ++ instanceElmType.typeName ++ "': " ++ error)

                                                            Ok instantiatedTypeResolution ->
                                                                functionNamesAndTypeParametersFromTypeText instantiatedTypeResolution.canonicalTypeText
                                                                    |> Result.map
                                                                        (\instantiatedTypeFunctionNames ->
                                                                            continueWithComponents
                                                                                { instantiatedTypeCanonicalName = instantiatedTypeResolution.canonicalTypeText
                                                                                , instantiatedTypeFunctionNameCommonPart = instantiatedTypeFunctionNames.commonPart
                                                                                , dependenciesFromInstantiatedType = Set.singleton instantiatedTypeResolution.canonicalTypeText
                                                                                }
                                                                        )

                                RecordElmType recordType ->
                                    {-
                                       logWriteLine?.Invoke("'" + rootTypeText + "' is a record type.");

                                       var fields = rootType.Record.Value.fields.Select(recordField =>
                                           {
                                               var fieldTypeResolution = ResolveLocalTypeText(recordField.typeText);

                                               var (fieldFunctionNames, fieldTypeParametersNames) =
                                                   GetFunctionNamesAndTypeParametersFromTypeText(fieldTypeResolution.canonicalTypeText);

                                               var encodeFunction = expressionTextForFunctionWithOptionalTypeParameters(
                                                   fieldFunctionNames.encodeFunctionName, jsonEncodeFunctionNamePrefix, fieldTypeParametersNames);

                                               var decodeFunction = expressionTextForFunctionWithOptionalTypeParameters(
                                                   fieldFunctionNames.decodeFunctionName, jsonDecodeFunctionNamePrefix, fieldTypeParametersNames);

                                               var encodeFieldValueExpression =
                                                   encodeParamName + "." + recordField.name + " |> " + encodeFunction;

                                               return new
                                               {
                                                   fieldName = recordField.name,
                                                   fieldCanonicalType = fieldTypeResolution.canonicalTypeText,
                                                   encodeExpression = "( \"" + recordField.name + "\", " + encodeFieldValueExpression + " )",
                                                   decodeExpression = "|> jsonDecode_andMap ( Json.Decode.field \"" + recordField.name + "\" " + decodeFunction + " )",
                                                   dependencies = fieldTypeResolution.dependencies,
                                               };
                                           }).ToImmutableList();

                                       var encodeListExpression =
                                           "[ " +
                                           String.Join(
                                               "\n, ",
                                               fields.Select(field => field.encodeExpression))
                                           + "\n]";

                                       var dencodeListExpression =
                                           String.Join("\n", fields.Select(field => field.decodeExpression));

                                       var allFieldsDependencies =
                                           fields
                                           .Select(field => field.dependencies)
                                           .Aggregate(ImmutableHashSet<string>.Empty, (a, b) => a.Union(b));

                                       var encodeExpression =
                                           "Json.Encode.object\n" +
                                           IndentElmCodeLines(1, encodeListExpression);

                                       var recordFieldsNames = rootType.Record.Value.fields.Select(field => field.name);

                                       var decodeMapFunction =
                                           "(\\" + String.Join(" ", recordFieldsNames) + " -> { " +
                                           String.Join(", ", recordFieldsNames.Select(fieldName => fieldName + " = " + fieldName))
                                           + " })";

                                       var decodeExpression =
                                           "Json.Decode.succeed " + decodeMapFunction + "\n" +
                                           IndentElmCodeLines(1, dencodeListExpression);

                                       var canonicalTypeText =
                                           "{" +
                                           string.Join(",", fields.Select(field => field.fieldName + ":" + field.fieldCanonicalType)) +
                                           "}";

                                       return new ResolveTypeResult
                                       {
                                           canonicalTypeText = canonicalTypeText,

                                           compileExpressions = () => (encodeExpression, decodeExpression, allFieldsDependencies),

                                           referencedModules = ImmutableHashSet<string>.Empty.Add(sourceModuleName),
                                       };
                                    -}
                                    let
                                        fieldsResult =
                                            recordType.fields
                                                |> List.map
                                                    (\recordField ->
                                                        resolveLocalTypeText recordField.typeText
                                                            |> Result.andThen
                                                                (\fieldTypeResolution ->
                                                                    functionNamesAndTypeParametersFromTypeText fieldTypeResolution.canonicalTypeText
                                                                        |> Result.map
                                                                            (\fieldFunctionNames ->
                                                                                let
                                                                                    encodeFunction =
                                                                                        expressionTextForFunctionWithOptionalTypeParameters
                                                                                            { functionName = fieldFunctionNames.encodeFunctionName
                                                                                            , typeParametersFunctionsCommonPrefix = jsonEncodeFunctionNamePrefix
                                                                                            , typeParametersNames = fieldFunctionNames.typeParametersNames
                                                                                            }

                                                                                    decodeFunction =
                                                                                        expressionTextForFunctionWithOptionalTypeParameters
                                                                                            { functionName = fieldFunctionNames.decodeFunctionName
                                                                                            , typeParametersFunctionsCommonPrefix = jsonDecodeFunctionNamePrefix
                                                                                            , typeParametersNames = fieldFunctionNames.typeParametersNames
                                                                                            }

                                                                                    encodeFieldValueExpression =
                                                                                        jsonEncodeParamName ++ "." ++ recordField.name ++ " |> " ++ encodeFunction
                                                                                in
                                                                                { fieldName = recordField.name
                                                                                , fieldCanonicalType = fieldTypeResolution.canonicalTypeText
                                                                                , encodeExpression = "( \"" ++ recordField.name ++ "\", " ++ encodeFieldValueExpression ++ " )"
                                                                                , decodeExpression = "|> jsonDecode_andMap ( Json.Decode.field \"" ++ recordField.name ++ "\" " ++ decodeFunction ++ " )"
                                                                                , dependencies = fieldTypeResolution.dependencies
                                                                                }
                                                                            )
                                                                )
                                                    )
                                                |> Result.Extra.combine
                                    in
                                    case fieldsResult of
                                        Err error ->
                                            Err ("Failed to resolve fields: " ++ error)

                                        Ok fields ->
                                            let
                                                encodeListExpression =
                                                    "["
                                                        ++ (fields |> List.map .encodeExpression |> String.join ",\n")
                                                        ++ "\n]"

                                                dencodeListExpression =
                                                    fields |> List.map .decodeExpression |> String.join "\n"

                                                allFieldsDependencies =
                                                    fields
                                                        |> List.map .dependencies
                                                        |> List.foldl Set.union Set.empty

                                                encodeExpression =
                                                    "Json.Encode.object\n"
                                                        ++ indentElmCodeLines 1 encodeListExpression

                                                recordFieldsNames =
                                                    recordType.fields |> List.map .name

                                                decodeMapFunction =
                                                    "(\\"
                                                        ++ String.join " " recordFieldsNames
                                                        ++ " -> { "
                                                        ++ String.join ", " (recordFieldsNames |> List.map (\fieldName -> fieldName ++ " = " ++ fieldName))
                                                        ++ " })"

                                                decodeExpression =
                                                    "Json.Decode.succeed "
                                                        ++ decodeMapFunction
                                                        ++ "\n"
                                                        ++ indentElmCodeLines 1 dencodeListExpression

                                                canonicalTypeText =
                                                    "{"
                                                        ++ String.join "," (fields |> List.map (\field -> field.fieldName ++ ":" ++ field.fieldCanonicalType))
                                                        ++ "}"
                                            in
                                            Ok
                                                { canonicalTypeText = canonicalTypeText
                                                , canonicalTypeTextWithParameters = canonicalTypeText
                                                , compileExpressions =
                                                    \() ->
                                                        Ok
                                                            { encodeExpression = encodeExpression
                                                            , decodeExpression = decodeExpression
                                                            , dependencies = allFieldsDependencies
                                                            }
                                                , referencedModules = maybeExplicitSourceModuleName |> Maybe.map Set.singleton |> Maybe.withDefault Set.empty
                                                }

                                CustomElmType customType ->
                                    jsonCodingResolveTypeCustomType customType

                                TupleElmType tupleElements ->
                                    {-
                                       var tupleElementsResults =
                                           rootType.Tuple
                                           .Select(tupleElementType => ResolveLocalTypeText(tupleElementType))
                                           .ToImmutableList();

                                       var tupleElementsCanonicalTypeName =
                                           tupleElementsResults
                                           .Select(elementResult => elementResult.canonicalTypeText)
                                           .ToImmutableList();

                                       var allElementsDependencies =
                                           tupleElementsResults.SelectMany(elementResult => elementResult.dependencies)
                                           .ToImmutableHashSet();

                                       var tupleElementsFunctionNames =
                                           tupleElementsCanonicalTypeName
                                           .Select(tupleElement => GetFunctionNamesAndTypeParametersFromTypeText(tupleElement).functionNames)
                                           .ToImmutableList();

                                       var functionNameCommonPart = jsonCodeTupleFunctionNameCommonPart + rootType.Tuple.Count.ToString();

                                       var encodeExpression =
                                           jsonEncodeFunctionNamePrefix + functionNameCommonPart + " " +
                                           String.Join(" ", tupleElementsFunctionNames.Select(functionNamesAndTypeParams =>
                                               functionNamesAndTypeParams.encodeFunctionName)) + " " +
                                           encodeParamName;

                                       var decodeExpression =
                                           jsonDecodeFunctionNamePrefix + functionNameCommonPart + " " +
                                           String.Join(" ", tupleElementsFunctionNames.Select(functionNamesAndTypeParams =>
                                               functionNamesAndTypeParams.decodeFunctionName));

                                       var canonicalTypeText = "(" + string.Join(",", tupleElementsCanonicalTypeName) + ")";

                                       return new ResolveTypeResult
                                       {
                                           canonicalTypeText = canonicalTypeText,

                                           compileExpressions = () => (encodeExpression, decodeExpression, allElementsDependencies),

                                           referencedModules = ImmutableHashSet<string>.Empty.Add(sourceModuleName),
                                       };
                                    -}
                                    case tupleElements |> List.map resolveLocalTypeText |> Result.Extra.combine of
                                        Err error ->
                                            Err ("Failed to resolve tuple element type text: " ++ error)

                                        Ok tupleElementsResults ->
                                            tupleElementsResults
                                                |> List.map (.canonicalTypeText >> functionNamesAndTypeParametersFromTypeText)
                                                |> Result.Extra.combine
                                                |> Result.map
                                                    (\tupleElementsFunctionNames ->
                                                        let
                                                            allElementsDependencies =
                                                                tupleElementsResults
                                                                    |> List.map .dependencies
                                                                    |> List.foldl Set.union Set.empty

                                                            tupleElementsCanonicalTypeName =
                                                                tupleElementsResults |> List.map .canonicalTypeText

                                                            functionNameCommonPart =
                                                                jsonCodeTupleFunctionNameCommonPart
                                                                    ++ String.fromInt (List.length tupleElements)

                                                            encodeExpression =
                                                                jsonEncodeFunctionNamePrefix
                                                                    ++ functionNameCommonPart
                                                                    ++ " "
                                                                    ++ (String.join " " (tupleElementsFunctionNames |> List.map .encodeFunctionName) ++ " ")
                                                                    ++ jsonEncodeParamName

                                                            decodeExpression =
                                                                jsonDecodeFunctionNamePrefix
                                                                    ++ functionNameCommonPart
                                                                    ++ " "
                                                                    ++ String.join " " (tupleElementsFunctionNames |> List.map .decodeFunctionName)

                                                            canonicalTypeText =
                                                                "(" ++ String.join "," tupleElementsCanonicalTypeName ++ ")"
                                                        in
                                                        { canonicalTypeText = canonicalTypeText
                                                        , canonicalTypeTextWithParameters = canonicalTypeText
                                                        , compileExpressions =
                                                            \() ->
                                                                Ok
                                                                    { encodeExpression = encodeExpression
                                                                    , decodeExpression = decodeExpression
                                                                    , dependencies = allElementsDependencies
                                                                    }
                                                        , referencedModules = maybeExplicitSourceModuleName |> Maybe.map Set.singleton |> Maybe.withDefault Set.empty
                                                        }
                                                    )

                                AliasElmType parsedAlias ->
                                    jsonCodingResolveType
                                        { rootTypeText = parsedAlias.aliasedText
                                        , maybeExplicitSourceModuleName = maybeExplicitSourceModuleName
                                        , rootSourceModuleText = rootSourceModuleText
                                        }
                                        sourceModules
        )
        sourceModuleText



{-
   static string expressionTextForFunctionWithOptionalTypeParameters(
       string functionName, string typeParametersFunctionsCommonPrefix, IImmutableList<string> typeParametersNames)
   {
       var needsParentheses = 0 < typeParametersNames.Count;

       var beforeParentheses =
           functionName +
           String.Join("", typeParametersNames.Select(typeParameterName => " " + typeParametersFunctionsCommonPrefix + "type_parameter_" + typeParameterName));

       if (needsParentheses)
           return "(" + beforeParentheses + ")";

       return beforeParentheses;
   }
-}


expressionTextForFunctionWithOptionalTypeParameters :
    { functionName : String
    , typeParametersFunctionsCommonPrefix : String
    , typeParametersNames : List String
    }
    -> String
expressionTextForFunctionWithOptionalTypeParameters { functionName, typeParametersFunctionsCommonPrefix, typeParametersNames } =
    let
        needsParentheses =
            typeParametersNames /= []

        beforeParentheses =
            functionName
                ++ " "
                ++ (typeParametersNames
                        |> List.map ((++) (typeParametersFunctionsCommonPrefix ++ "type_parameter_"))
                        |> String.join " "
                   )
    in
    if needsParentheses then
        "(" ++ beforeParentheses ++ ")"

    else
        beforeParentheses


getTypeDefinitionTextFromModuleText : String -> String -> Result String (Maybe String)
getTypeDefinitionTextFromModuleText typeNameInModule moduleText =
    moduleText
        |> parseElmModuleText
        |> Result.map
            (\parsedModule ->
                parsedModule.declarations
                    |> List.filter
                        (\declaration ->
                            case Elm.Syntax.Node.value declaration of
                                Elm.Syntax.Declaration.CustomTypeDeclaration customTypeDeclaration ->
                                    Elm.Syntax.Node.value customTypeDeclaration.name == typeNameInModule

                                Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
                                    Elm.Syntax.Node.value aliasDeclaration.name == typeNameInModule

                                _ ->
                                    False
                        )
                    |> List.head
                    |> Maybe.map
                        (\declaration ->
                            getTextLinesFromRange (Elm.Syntax.Node.range declaration) moduleText
                                |> String.join "\n"
                        )
            )
        |> Result.mapError (parserDeadEndsToString moduleText)



{-
   /*
   Resolve the 'exposing' and optional 'as' parts 'import' statements.
   Return null in case this name is not imported.
   */
   static public (string moduleName, string localTypeName)? GetCanonicalNameFromImportedNameInModule(
       string importedNameInModule, string moduleText)
   {
       //  Assume: Each import is expressed on a single line.

       var qualifiedMatch = Regex.Match(importedNameInModule, @"(.+)\.(.+)");

       var imports =
           GetElmModuleTextLines(moduleText)
           .Select(line => Regex.Match(line, @"^import\s+([\w\d\._]+)(.*)"))
           .Where(match => match.Success)
           .ToImmutableDictionary(match => match.Groups[1].Value, match => match.Groups[2].Value);

       if (qualifiedMatch.Success)
       {
           //  In this case, we are looking for 'as' or the unmapped name.

           var moduleName = qualifiedMatch.Groups[1].Value;
           var typeName = qualifiedMatch.Groups[2].Value;

           {
               imports.TryGetValue(moduleName, out var literalMatch);

               if (literalMatch != null)
                   return (moduleName, typeName);
           }

           {
               var matchingImportByAs =
                   imports.FirstOrDefault(import =>
                   {
                       var asMatch = Regex.Match(import.Value, @"^\s+as\s+([\w\d_]+)");

                       return asMatch.Success && asMatch.Groups[1].Value == moduleName;
                   });

               if (matchingImportByAs.Key != null)
                   return (matchingImportByAs.Key, typeName);
           }

           return null;
       }
       else
       {
           foreach (var import in imports)
           {
               var exposingMatch = Regex.Match(import.Value, @"^\s*(|as\s+[\w\d_]+\s*)exposing\s*\(([^\)]*)\)");

               if (!exposingMatch.Success)
                   continue;

               var exposedAggregated = exposingMatch.Groups[2].Value.Trim();

               if (exposedAggregated == "..")
               {
                   //  resolving 'exposing(..)' in import is not implemented yet.

                   continue;
               }

               var exposedNames =
                   exposedAggregated
                   .Split(new[] { ',' })
                   .Select(commaSeparatedValue => commaSeparatedValue.Trim())
                   .ToImmutableList();

               if (exposedNames.Contains(importedNameInModule))
               {
                   return (import.Key, importedNameInModule);
               }
           }

           return null;
       }
   }
-}


getCanonicalNameFromImportedNameInModule : Elm.Syntax.File.File -> String -> Maybe ( List String, String )
getCanonicalNameFromImportedNameInModule parsedModule nameInModuleBeforeTrimming =
    let
        nameInModule =
            String.trim nameInModuleBeforeTrimming

        importedModuleAlias =
            nameInModule |> String.split "." |> List.reverse |> List.drop 1 |> List.reverse

        localName =
            nameInModule |> String.split "." |> List.reverse |> List.head |> Maybe.withDefault nameInModule
    in
    if importedModuleAlias == [] then
        parsedModule.imports
            |> List.map Elm.Syntax.Node.value
            |> List.filterMap
                (\moduleImport ->
                    case Maybe.map Elm.Syntax.Node.value moduleImport.exposingList of
                        Nothing ->
                            Nothing

                        Just importExposing ->
                            let
                                containsMatchingExposition =
                                    case importExposing of
                                        Elm.Syntax.Exposing.All _ ->
                                            -- TODO: Add lookup into that module
                                            False

                                        Elm.Syntax.Exposing.Explicit topLevelExpositions ->
                                            topLevelExpositions
                                                |> List.any
                                                    (\topLevelExpose ->
                                                        case Elm.Syntax.Node.value topLevelExpose of
                                                            Elm.Syntax.Exposing.InfixExpose _ ->
                                                                False

                                                            Elm.Syntax.Exposing.FunctionExpose _ ->
                                                                False

                                                            Elm.Syntax.Exposing.TypeOrAliasExpose typeOrAliasExpose ->
                                                                typeOrAliasExpose == localName

                                                            Elm.Syntax.Exposing.TypeExpose typeExpose ->
                                                                typeExpose.name == localName
                                                    )
                            in
                            if containsMatchingExposition then
                                Just ( Elm.Syntax.Node.value moduleImport.moduleName, localName )

                            else
                                Nothing
                )
            |> List.head

    else
        case
            parsedModule.imports
                |> List.map Elm.Syntax.Node.value
                |> List.filter
                    (\moduleImport ->
                        moduleImport.moduleAlias
                            |> Maybe.withDefault moduleImport.moduleName
                            |> Elm.Syntax.Node.value
                            |> (==) importedModuleAlias
                    )
                |> List.head
        of
            Nothing ->
                Just ( importedModuleAlias, localName )

            Just matchingImport ->
                Just ( Elm.Syntax.Node.value matchingImport.moduleName, localName )


{-| TODO: Test simplify by reusing parsed representation in parseElmTypeText.
-}
parseElmTypeText : Bool -> String -> Result String ( ElmTypeStructure, String )
parseElmTypeText canBeInstance typeTextBeforeTrimming =
    let
        typeText =
            String.trim typeTextBeforeTrimming

        typeDefinitionTextLines =
            String.lines typeText
                |> List.filter (String.trim >> String.isEmpty >> not)
    in
    case
        typeDefinitionTextLines
            |> List.head
            |> Maybe.map (String.split " " >> List.map String.trim)
    of
        Just ("type" :: "alias" :: aliasName :: parametersAndAssignmentSymbol) ->
            case List.reverse parametersAndAssignmentSymbol of
                "=" :: parametersReversed ->
                    Ok
                        ( AliasElmType
                            { aliasLocalName = aliasName
                            , parameters = List.reverse parametersReversed
                            , aliasedText = typeDefinitionTextLines |> List.drop 1 |> String.join "\n"
                            }
                        , ""
                        )

                _ ->
                    Err ("Unexpected text after alias: '" ++ typeText ++ "'")

        Just ("type" :: customTypeName :: parameters) ->
            -- Assume: Custom type tags are all on their own line.
            let
                tagsResults =
                    typeDefinitionTextLines
                        |> List.drop 1
                        |> List.map
                            (\tagLine ->
                                let
                                    ( _, textAfterTagSeparator ) =
                                        parseRegexPattern { regexPattern = "(\\||\\=)" } (String.trim tagLine)

                                    ( tagName, textAfterTagName ) =
                                        parseRegexPattern { regexPattern = "[^\\s]+" } (String.trim textAfterTagSeparator)

                                    parseTags remainingText =
                                        if remainingText == "" then
                                            Ok []

                                        else
                                            parseElmTypeText False remainingText
                                                |> Result.andThen
                                                    (\( _, remainingAfterCurrentParam ) ->
                                                        let
                                                            paramText =
                                                                remainingText
                                                                    |> String.left (String.length remainingText - String.length remainingAfterCurrentParam)
                                                                    |> String.trim
                                                        in
                                                        parseTags (String.trim remainingAfterCurrentParam)
                                                            |> Result.map (\followingParams -> paramText :: followingParams)
                                                    )
                                in
                                case parseTags (String.trim textAfterTagName) of
                                    Err error ->
                                        Err ("Failed to parse custom type params: " ++ error)

                                    Ok params ->
                                        Ok ( tagName, params )
                            )
            in
            case Result.Extra.combine tagsResults of
                Err error ->
                    Err ("Failed to parse tags: " ++ error)

                Ok tags ->
                    Ok
                        ( CustomElmType
                            { typeLocalName = customTypeName
                            , parameters = parameters
                            , tags = Dict.fromList tags
                            }
                        , ""
                        )

        _ ->
            case typeText |> String.toList |> List.head of
                Nothing ->
                    Err "Type text is empty"

                Just '(' ->
                    let
                        parseElements : String -> Result String ( List { typeText : String, parsedType : ElmTypeStructure }, String )
                        parseElements remainingElementsText =
                            if String.startsWith ")" remainingElementsText then
                                Ok ( [], String.dropLeft 1 remainingElementsText )

                            else if remainingElementsText == "" then
                                Err "Missing tuple/instance termination token."

                            else
                                case parseElmTypeText True remainingElementsText of
                                    Err error ->
                                        Err ("Failed to parse tuple element type: " ++ error)

                                    Ok ( fieldType, restAfterElementType ) ->
                                        let
                                            restAfterElementTypeTrimmed =
                                                String.trimLeft restAfterElementType

                                            elementTypeTextLength =
                                                String.length remainingElementsText - String.length restAfterElementType

                                            currentElement =
                                                { parsedType = fieldType
                                                , typeText =
                                                    remainingElementsText
                                                        |> String.left elementTypeTextLength
                                                        |> String.trim
                                                }

                                            continueForText textForRemainingElements =
                                                parseElements (String.trimLeft textForRemainingElements)
                                                    |> Result.map
                                                        (\( followingElements, remainingAfterElements ) ->
                                                            ( currentElement :: followingElements
                                                            , remainingAfterElements
                                                            )
                                                        )
                                        in
                                        case restAfterElementTypeTrimmed |> String.toList |> List.head of
                                            Nothing ->
                                                Err "Missing tuple/instance termination token."

                                            Just ',' ->
                                                continueForText (String.dropLeft 1 restAfterElementTypeTrimmed)

                                            Just ')' ->
                                                continueForText restAfterElementTypeTrimmed

                                            Just otherChar ->
                                                Err ("Unexpected char in tuple/instance: '" ++ String.fromChar otherChar ++ "'")
                    in
                    parseElements (String.trimLeft (String.dropLeft 1 typeText))
                        |> Result.andThen
                            (\( elements, remainingAfterFields ) ->
                                case elements of
                                    [ singleElement ] ->
                                        case parseElmTypeText True singleElement.typeText of
                                            Err error ->
                                                Err ("Failed switching from parens to instance: " ++ error)

                                            Ok ( parsedInstance, _ ) ->
                                                Ok ( parsedInstance, remainingAfterFields )

                                    _ ->
                                        Ok ( TupleElmType (List.map .typeText elements), remainingAfterFields )
                            )

                Just '{' ->
                    let
                        parseFields : String -> Result String ( List { name : String, typeText : String, parsedType : ElmTypeStructure }, String )
                        parseFields remainingFieldsText =
                            if String.startsWith "}" remainingFieldsText then
                                Ok ( [], String.dropLeft 1 remainingFieldsText )

                            else if remainingFieldsText == "" then
                                Err ("Missing record termination token in '" ++ typeText ++ "'")

                            else
                                let
                                    ( fieldName, restAfterFieldName ) =
                                        parseFieldName remainingFieldsText

                                    ( _, restAfterFieldColon ) =
                                        parseRegexPattern { regexPattern = "^\\s*:\\s*" }
                                            restAfterFieldName
                                in
                                case parseElmTypeText True restAfterFieldColon of
                                    Err error ->
                                        Err ("Failed to parse field type: " ++ error)

                                    Ok ( fieldType, restAfterFieldType ) ->
                                        let
                                            fieldTypeTextLength =
                                                String.length restAfterFieldColon - String.length restAfterFieldType

                                            ( _, restAfterFieldSeparator ) =
                                                parseRegexPattern { regexPattern = "^\\s*,\\s*" }
                                                    restAfterFieldType
                                        in
                                        parseFields (String.trimLeft restAfterFieldSeparator)
                                            |> Result.map
                                                (\( followingFields, remainingAfterFields ) ->
                                                    ( { name = fieldName
                                                      , parsedType = fieldType
                                                      , typeText = restAfterFieldColon |> String.left fieldTypeTextLength |> String.trim
                                                      }
                                                        :: followingFields
                                                    , remainingAfterFields
                                                    )
                                                )
                    in
                    parseFields (String.trimLeft (String.dropLeft 1 typeText))
                        |> Result.map
                            (\( fields, remainingAfterFields ) ->
                                ( RecordElmType { fields = fields }, remainingAfterFields )
                            )

                Just _ ->
                    let
                        nameInInstanceRegexPattern =
                            "^[\\w\\d_\\.]+"

                        ( firstName, restAfterFirstName ) =
                            parseRegexPattern { regexPattern = nameInInstanceRegexPattern }
                                typeText

                        parseParameters remainingParametersText =
                            if String.length remainingParametersText < 1 then
                                Ok ( [], remainingParametersText )

                            else if
                                String.startsWith "[" remainingParametersText
                                    || String.startsWith "(" remainingParametersText
                                    || String.startsWith "{" remainingParametersText
                            then
                                case parseElmTypeText True remainingParametersText of
                                    Err error ->
                                        Err ("Failed to parse instance parameter: " ++ error)

                                    Ok ( currentParameterType, remainingAfterCurrentParameter ) ->
                                        let
                                            parameterTypeTextLength =
                                                String.length remainingParametersText - String.length remainingAfterCurrentParameter

                                            parameterTypeText =
                                                remainingParametersText |> String.left parameterTypeTextLength
                                        in
                                        parseParameters (String.trimLeft remainingAfterCurrentParameter)
                                            |> Result.map
                                                (\( followingParameters, lastRemaining ) ->
                                                    ( parameterTypeText :: followingParameters
                                                    , lastRemaining
                                                    )
                                                )

                            else
                                let
                                    ( parameterName, restAfterParameterName ) =
                                        parseRegexPattern
                                            { regexPattern = nameInInstanceRegexPattern }
                                            remainingParametersText
                                in
                                if parameterName == "" then
                                    Ok ( [], restAfterParameterName )

                                else
                                    case parseElmTypeText False remainingParametersText of
                                        Err error ->
                                            Err ("Failed to parse instance parameter: " ++ error)

                                        Ok ( currentParameterType, remainingAfterCurrentParameter ) ->
                                            let
                                                parameterTypeTextLength =
                                                    String.length remainingParametersText - String.length remainingAfterCurrentParameter

                                                parameterTypeText =
                                                    remainingParametersText |> String.left parameterTypeTextLength
                                            in
                                            parseParameters (String.trimLeft remainingAfterCurrentParameter)
                                                |> Result.map
                                                    (\( followingParameters, lastRemaining ) ->
                                                        ( parameterTypeText :: followingParameters
                                                        , lastRemaining
                                                        )
                                                    )
                    in
                    if not canBeInstance then
                        Ok ( InstanceElmType { typeName = firstName, parameters = [] }, restAfterFirstName )

                    else
                        -- Err ("parseElmTypeText not implemented yet for '" ++ typeText ++ "'")
                        parseParameters (String.trimLeft restAfterFirstName)
                            |> Result.map
                                (\( parameters, remainingAfterParameters ) ->
                                    ( InstanceElmType { typeName = firstName, parameters = parameters }
                                    , remainingAfterParameters
                                    )
                                )


parseFieldName : String -> ( String, String )
parseFieldName =
    parseRegexPattern { regexPattern = "[\\w\\d_]+" }


parseRegexPattern : { regexPattern : String } -> String -> ( String, String )
parseRegexPattern { regexPattern } originalString =
    case Regex.fromString regexPattern of
        Nothing ->
            ( "", originalString )

        Just regex ->
            case Regex.find regex originalString of
                [] ->
                    ( "", originalString )

                firstMatch :: _ ->
                    ( firstMatch.match
                    , originalString |> String.dropLeft (firstMatch.index + String.length firstMatch.match)
                    )


enumerateExpressionsResolvingAllDependencies : (String -> Result String ResolveTypeResult) -> Set.Set String -> Result String (List { elmType : String, expressions : ExpressionsForType })
enumerateExpressionsResolvingAllDependencies =
    enumerateExpressionsResolvingAllDependenciesIgnoringTypes { typesToIgnore = Set.empty }


enumerateExpressionsResolvingAllDependenciesIgnoringTypes : { typesToIgnore : Set.Set String } -> (String -> Result String ResolveTypeResult) -> Set.Set String -> Result String (List { elmType : String, expressions : ExpressionsForType })
enumerateExpressionsResolvingAllDependenciesIgnoringTypes { typesToIgnore } getExpressionsAndDependenciesForType rootTypes =
    Set.diff rootTypes typesToIgnore
        |> Set.toList
        |> List.map
            (\rootType ->
                case getExpressionsAndDependenciesForType rootType of
                    Err error ->
                        Err error

                    Ok expressionsAndDependenciesAndCanonicalName ->
                        case expressionsAndDependenciesAndCanonicalName.compileExpressions () of
                            Err error ->
                                Err ("Failed to compile expressions: " ++ error)

                            Ok expressionsAndDependencies ->
                                case
                                    enumerateExpressionsResolvingAllDependenciesIgnoringTypes
                                        { typesToIgnore = Set.union (Set.singleton rootType) typesToIgnore }
                                        getExpressionsAndDependenciesForType
                                        expressionsAndDependencies.dependencies
                                of
                                    Err error ->
                                        Err ("Failed to get dependency expression: " ++ error)

                                    Ok dependenciesItems ->
                                        -- Err "enumerateExpressionsResolvingAllDependenciesIgnoringTypes not implemented"
                                        Ok
                                            ({ elmType = expressionsAndDependenciesAndCanonicalName.canonicalTypeTextWithParameters
                                             , expressions =
                                                { encodeExpression = expressionsAndDependencies.encodeExpression
                                                , decodeExpression = expressionsAndDependencies.decodeExpression
                                                , referencedModules = expressionsAndDependenciesAndCanonicalName.referencedModules
                                                }
                                             }
                                                :: dependenciesItems
                                            )
            )
        |> Result.Extra.combine
        |> Result.map List.concat


jsonCodeLeafExpressions : Dict.Dict String ( String, String )
jsonCodeLeafExpressions =
    [ ( "String", ( "Json.Encode.string " ++ jsonEncodeParamName, "Json.Decode.string" ) )
    , ( "Int", ( "Json.Encode.int " ++ jsonEncodeParamName, "Json.Decode.int" ) )
    , ( "Bool", ( "Json.Encode.bool " ++ jsonEncodeParamName, "Json.Decode.bool" ) )
    , ( "Float", ( "Json.Encode.float " ++ jsonEncodeParamName, "Json.Decode.float" ) )
    , ( "()", ( "Json.Encode.list (always (Json.Encode.object [])) []", "Json.Decode.succeed ()" ) )
    , ( "{}", ( "Json.Encode.object []", "Json.Decode.succeed {}" ) )
    , ( "Bytes.Bytes", ( "json_encode_Bytes " ++ jsonEncodeParamName, "json_decode_Bytes" ) )
    ]
        |> Dict.fromList



{-
   static IImmutableDictionary<string, string> InstantiationSpecialCases =>
       ImmutableDictionary<string, string>.Empty
       .Add("List", jsonCodeListFunctionNameCommonPart)
       .Add("Set.Set", jsonCodeSetFunctionNameCommonPart)
       .Add("Maybe", jsonCodeMaybeFunctionNameCommonPart)
       .Add("Result", jsonCodeResultFunctionNameCommonPart)
       .Add("Dict.Dict", jsonCodeDictFunctionNameCommonPart);
-}


instantiationSpecialCases : Dict.Dict String String
instantiationSpecialCases =
    [ ( "List", jsonCodeListFunctionNameCommonPart )
    , ( "Set.Set", jsonCodeSetFunctionNameCommonPart )
    , ( "Maybe", jsonCodeMaybeFunctionNameCommonPart )
    , ( "Result", jsonCodeResultFunctionNameCommonPart )
    , ( "Dict.Dict", jsonCodeDictFunctionNameCommonPart )
    ]
        |> Dict.fromList


generalSupportingFunctionsTexts : List { functionName : String, functionText : String }
generalSupportingFunctionsTexts =
    (generalSupportingFunctionsTextsWithCommonNamePattern
        |> List.concatMap
            (\generalSupportingFunction ->
                [ { name =
                        jsonEncodeFunctionNamePrefix
                            ++ generalSupportingFunction.functionNameCommonPart
                  , afterName = generalSupportingFunction.encodeSyntax
                  }
                , { name =
                        jsonDecodeFunctionNamePrefix
                            ++ generalSupportingFunction.functionNameCommonPart
                  , afterName = generalSupportingFunction.decodeSyntax
                  }
                ]
            )
        |> List.map
            (\function ->
                { functionName = function.name
                , functionText = function.name ++ " " ++ function.afterName
                }
            )
    )
        ++ {-
              jsonDecode_andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
              jsonDecode_andMap =
                  Json.Decode.map2 (|>)
              ",
              $@"
              json_encode_Bytes : Bytes.Bytes -> Json.Encode.Value
              json_encode_Bytes bytes =
                  [ ( ""AsBase64"", bytes |> Base64.fromBytes |> Maybe.withDefault ""Error encoding to base64"" |> Json.Encode.string ) ]
                      |> Json.Encode.object
              ",
              $@"
              json_decode_Bytes : Json.Decode.Decoder Bytes.Bytes
              json_decode_Bytes =
                  Json.Decode.field ""AsBase64""
                      (Json.Decode.string
                          |> Json.Decode.andThen
                              (Base64.toBytes >> Maybe.map Json.Decode.succeed >> Maybe.withDefault (Json.Decode.fail ""Failed to decode base64.""))
                      )
              "
           -}
           [ { functionName = "jsonDecode_andMap"
             , functionText = """
jsonDecode_andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
jsonDecode_andMap =
    Json.Decode.map2 (|>)"""
             }
           , { functionName = "json_encode_Bytes"
             , functionText = """
json_encode_Bytes : Bytes.Bytes -> Json.Encode.Value
json_encode_Bytes bytes =
    [ ( "AsBase64", bytes |> Base64.fromBytes |> Maybe.withDefault "Error encoding to base64" |> Json.Encode.string ) ]
        |> Json.Encode.object"""
             }
           , { functionName = "json_decode_Bytes"
             , functionText = """
json_decode_Bytes : Json.Decode.Decoder Bytes.Bytes
json_decode_Bytes =
    Json.Decode.field "AsBase64"
        (Json.Decode.string
            |> Json.Decode.andThen
                (Base64.toBytes >> Maybe.map Json.Decode.succeed >> Maybe.withDefault (Json.Decode.fail "Failed to decode base64."))
        )
              """
             }
           ]


generalSupportingFunctionsTextsWithCommonNamePattern :
    List
        { functionNameCommonPart : String
        , encodeSyntax : String
        , decodeSyntax : String
        }
generalSupportingFunctionsTextsWithCommonNamePattern =
    [ { functionNameCommonPart = jsonCodeMaybeFunctionNameCommonPart
      , encodeSyntax = """encodeJust valueToEncode =
    case valueToEncode of
        Nothing ->
            [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object

        Just just ->
            [ ( "Just", [ just ] |> Json.Encode.list encodeJust ) ] |> Json.Encode.object
"""
      , decodeSyntax = """decoder =
    Json.Decode.oneOf
        [ Json.Decode.field "Nothing" (Json.Decode.succeed Nothing)
        , Json.Decode.field "Just" ((Json.Decode.index 0 decoder) |> Json.Decode.map Just)
        , Json.Decode.field "Just" (decoder |> Json.Decode.map Just) -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.null Nothing -- Temporary backwardscompatibility: Map 'null' to Nothing
        ]
     """
      }
    , { functionNameCommonPart = jsonCodeListFunctionNameCommonPart
      , encodeSyntax = """ = Json.Encode.list"""
      , decodeSyntax = """ = Json.Decode.list"""
      }
    , { functionNameCommonPart = jsonCodeSetFunctionNameCommonPart
      , encodeSyntax = """encoder =
    Set.toList >> Json.Encode.list encoder"""
      , decodeSyntax = """decoder =
    Json.Decode.list decoder |> Json.Decode.map Set.fromList"""
      }
    , { functionNameCommonPart = jsonCodeDictFunctionNameCommonPart
      , encodeSyntax = """encodeKey encodeValue =
    Dict.toList >> Json.Encode.list (""" ++ jsonEncodeFunctionNamePrefix ++ jsonCodeTupleFunctionNameCommonPart ++ "2 encodeKey encodeValue)"
      , decodeSyntax = """decodeKey decodeValue =
        (Json.Decode.list (""" ++ jsonDecodeFunctionNamePrefix ++ jsonCodeTupleFunctionNameCommonPart ++ """2 decodeKey decodeValue))
            |> Json.Decode.map Dict.fromList"""
      }
    , { functionNameCommonPart = jsonCodeResultFunctionNameCommonPart
      , encodeSyntax = """encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( "Err", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( "Ok", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object"""
      , decodeSyntax = """decodeErr decodeOk =
    Json.Decode.oneOf
        [ Json.Decode.field "Err" (Json.Decode.index 0 decodeErr) |> Json.Decode.map Err
        , Json.Decode.field "Ok" (Json.Decode.index 0 decodeOk) |> Json.Decode.map Ok
        , Json.Decode.field "Err" decodeErr |> Json.Decode.map Err -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.field "Ok" decodeOk |> Json.Decode.map Ok -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        ]"""
      }
    , { functionNameCommonPart = jsonCodeTupleFunctionNameCommonPart ++ "2"
      , encodeSyntax = """encodeA encodeB ( a, b ) =
    [ a |> encodeA, b |> encodeB ]
        |> Json.Encode.list identity"""
      , decodeSyntax = """decodeA decodeB =
    Json.Decode.map2 (\\a b -> ( a, b ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)"""
      }
    , { functionNameCommonPart = jsonCodeTupleFunctionNameCommonPart ++ "3"
      , encodeSyntax = """encodeA encodeB encodeC ( a, b, c ) =
    [ a |> encodeA, b |> encodeB, c |> encodeC ]
        |> Json.Encode.list identity"""
      , decodeSyntax = """decodeA decodeB decodeC =
    Json.Decode.map3 (\\a b c -> ( a, b, c ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)
        (Json.Decode.index 2 decodeC)"""
      }
    ]


jsonCodeMaybeFunctionNameCommonPart : String
jsonCodeMaybeFunctionNameCommonPart =
    "_generic_Maybe"


jsonCodeListFunctionNameCommonPart : String
jsonCodeListFunctionNameCommonPart =
    "_generic_List"


jsonCodeSetFunctionNameCommonPart : String
jsonCodeSetFunctionNameCommonPart =
    "_generic_Set"


jsonCodeDictFunctionNameCommonPart : String
jsonCodeDictFunctionNameCommonPart =
    "_generic_Dict"


jsonCodeResultFunctionNameCommonPart : String
jsonCodeResultFunctionNameCommonPart =
    "_generic_Result"


jsonCodeTupleFunctionNameCommonPart : String
jsonCodeTupleFunctionNameCommonPart =
    "_tuple_"


jsonEncodeParamName : String
jsonEncodeParamName =
    "valueToEncode"


functionNamesAndTypeParametersFromTypeText :
    String
    ->
        Result
            String
            { encodeFunctionName : String
            , decodeFunctionName : String
            , commonPart : String
            , typeParametersNames : List String
            }
functionNamesAndTypeParametersFromTypeText typeText =
    parseForTypeParameters typeText
        |> Result.map
            (\( typeTextMinusTypeParameters, typeParametersNames ) ->
                let
                    rootTypeTextHash =
                        SHA256.fromString typeTextMinusTypeParameters

                    functionNameCommonPart =
                        if stringStartsWithLowercaseLetter (String.trim typeTextMinusTypeParameters) then
                            "type_parameter_" ++ String.trim typeTextMinusTypeParameters

                        else
                            -- TODO: Implement nicer names
                            "anonymous_" ++ String.left 10 (SHA256.toHex rootTypeTextHash)
                in
                { encodeFunctionName = jsonEncodeFunctionNamePrefix ++ functionNameCommonPart
                , decodeFunctionName = jsonDecodeFunctionNamePrefix ++ functionNameCommonPart
                , commonPart = functionNameCommonPart
                , typeParametersNames = typeParametersNames
                }
            )



{-
   static (string typeTextMinusTypeParameters, ImmutableList<string> typeParametersNames) parseForTypeParameters(string typeText)
   {
       var instanceTypeTextWithParametersMatch = Regex.Match(typeText.Trim(), @"^\((.+?)((\s+[a-z][^\s]*){1,})\)$");

       if (instanceTypeTextWithParametersMatch.Success)
           return
           (instanceTypeTextWithParametersMatch.Groups[1].Value,
           Regex.Split(instanceTypeTextWithParametersMatch.Groups[2].Value.Trim(), @"\s+").ToImmutableList());

       var parseTypeResult = ParseElmTypeText(typeText, canBeInstance: true);

       var parsedType = parseTypeResult.parsedType;

       if (parsedType.Instance != null)
       {
           var typeParametersNames =
               parsedType.Instance.Value.parameters
               .SelectMany(EnumerateAllTypeNamesFromTypeText)
               .Where(typeName => Char.IsLower(typeName[0])).ToImmutableList();

           return (typeText, typeParametersNames);
       }

       {
           var typeParametersNames =
               EnumerateAllTypeNamesFromTypeText(typeText).Where(typeName => Char.IsLower(typeName[0])).ToImmutableList();

           return (typeText, typeParametersNames);
       }
   }
-}


parseForTypeParameters : String -> Result String ( String, List String )
parseForTypeParameters typeText =
    case Regex.fromString "^\\((.+?)((\\s+[a-z][^\\s]*){1,})\\)$" of
        Nothing ->
            Err "Failed to build regex in parseForTypeParameters"

        Just regex ->
            case Regex.find regex (String.trim typeText) of
                firstMatch :: _ ->
                    case firstMatch.submatches of
                        (Just instancedTypeName) :: _ ->
                            Ok
                                ( String.trim instancedTypeName
                                , String.trim firstMatch.match
                                    |> String.dropRight 1
                                    |> String.dropLeft (firstMatch.index + String.length instancedTypeName + 1)
                                    |> String.split " "
                                    |> List.map String.trim
                                    |> List.filter (String.isEmpty >> not)
                                )

                        _ ->
                            Err ("Unexpected structure in regex match (" ++ String.fromInt (List.length firstMatch.submatches) ++ " submatches)")

                _ ->
                    parseElmTypeText True typeText
                        |> Result.andThen
                            (\( parsedType, _ ) ->
                                case parsedType of
                                    InstanceElmType instanceType ->
                                        instanceType.parameters
                                            |> List.map enumerateAllTypeNamesFromTypeText
                                            |> Result.Extra.combine
                                            |> Result.map List.concat
                                            |> Result.map
                                                (\typesNames -> ( typeText, typesNames |> List.filter stringStartsWithLowercaseLetter ))

                                    _ ->
                                        enumerateAllTypeNamesFromTypeText typeText
                                            |> Result.map
                                                (\typesNames ->
                                                    ( typeText, typesNames |> List.filter stringStartsWithLowercaseLetter )
                                                )
                            )



{-
      static IEnumerable<string> EnumerateAllTypeNamesFromTypeText(string typeText)
   {
       var parseTypeResult = ParseElmTypeText(typeText, canBeInstance: true);

       if (0 < parseTypeResult.remainingString?.Trim()?.Length)
           throw new NotSupportedException("Unexpected remaining string after parsing type: '" + parseTypeResult.remainingString + "'.");

       var parsedType = parseTypeResult.parsedType;

       if (parsedType.Instance != null)
       {
           var instancedTypes = new[] { parsedType.Instance.Value.typeName };

           if (parsedType.Instance.Value.parameters.Count < 1)
               return instancedTypes;

           return instancedTypes.Concat(parsedType.Instance.Value.parameters.SelectMany(EnumerateAllTypeNamesFromTypeText));
       }

       if (parsedType.Tuple != null)
           return parsedType.Tuple.SelectMany(EnumerateAllTypeNamesFromTypeText);

       if (parsedType.Record != null)
           return parsedType.Record.Value.fields.SelectMany(field => EnumerateAllTypeNamesFromTypeText(field.typeText));

       throw new NotImplementedException();
   }
-}


enumerateAllTypeNamesFromTypeText : String -> Result String (List String)
enumerateAllTypeNamesFromTypeText typeText =
    parseElmTypeText True typeText
        |> Result.andThen
            (\( parsedType, remainingString ) ->
                if String.trim remainingString /= "" then
                    Err ("Unexpected remaining string after parsing type: '" ++ remainingString ++ "'.")

                else
                    case parsedType of
                        InstanceElmType instanceType ->
                            instanceType.parameters
                                |> List.map enumerateAllTypeNamesFromTypeText
                                |> Result.Extra.combine
                                |> Result.map List.concat
                                |> Result.map (List.filter stringStartsWithLowercaseLetter)
                                |> Result.map ((::) instanceType.typeName)

                        TupleElmType tupleType ->
                            tupleType
                                |> List.map enumerateAllTypeNamesFromTypeText
                                |> Result.Extra.combine
                                |> Result.map List.concat

                        RecordElmType recordType ->
                            recordType.fields
                                |> List.map (.typeText >> enumerateAllTypeNamesFromTypeText)
                                |> Result.Extra.combine
                                |> Result.map List.concat

                        _ ->
                            Err ("enumerateAllTypeNamesFromTypeText not implemeted for type '" ++ typeText ++ "'")
            )


jsonEncodeFunctionNamePrefix : String
jsonEncodeFunctionNamePrefix =
    "jsonEncode_"


jsonDecodeFunctionNamePrefix : String
jsonDecodeFunctionNamePrefix =
    "jsonDecode_"


elmModulesDictFromAppFiles : AppFiles -> Dict.Dict String String
elmModulesDictFromAppFiles appFiles =
    appFiles
        |> Dict.toList
        |> List.filterMap (Tuple.second >> stringFromFileContent)
        |> List.filterMap
            (\fileText ->
                case parseElmModuleText fileText of
                    Err _ ->
                        Nothing

                    Ok elmFile ->
                        Just
                            ( String.join "." (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value elmFile.moduleDefinition))
                            , fileText
                            )
            )
        |> Dict.fromList


parseJsonCodingFunctionType : String -> Elm.Syntax.Signature.Signature -> Result String { typeCanonicalName : String, isDecoder : Bool }
parseJsonCodingFunctionType moduleText signature =
    let
        errorValue detail =
            Err
                ("Unexpected type signature ("
                    ++ detail
                    ++ "): "
                    ++ (Elm.Syntax.Node.value signature.typeAnnotation
                            |> Elm.Syntax.TypeAnnotation.encode
                            |> Json.Encode.encode 0
                       )
                )
    in
    case Elm.Syntax.Node.value signature.typeAnnotation of
        Elm.Syntax.TypeAnnotation.Typed genericType typeArguments ->
            if Tuple.second (Elm.Syntax.Node.value genericType) /= "Decoder" then
                errorValue "Generic type is not 'Decoder'"

            else
                case typeArguments of
                    [ singleTypeArgument ] ->
                        Ok
                            { typeCanonicalName =
                                getTextLinesFromRange
                                    (Elm.Syntax.Node.range singleTypeArgument)
                                    moduleText
                                    |> String.join " "
                            , isDecoder = True
                            }

                    _ ->
                        errorValue "Unexpected number of type arguments for 'Decoder'"

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation leftType rightType ->
            case Elm.Syntax.Node.value rightType of
                Elm.Syntax.TypeAnnotation.Typed rightNameNode _ ->
                    if Tuple.second (Elm.Syntax.Node.value rightNameNode) /= "Value" then
                        errorValue "right side of function type is not 'Value'"

                    else
                        Ok
                            { typeCanonicalName =
                                getTextLinesFromRange
                                    (Elm.Syntax.Node.range leftType)
                                    moduleText
                                    |> String.join " "
                            , isDecoder = False
                            }

                _ ->
                    errorValue "right side of function type"

        _ ->
            errorValue ""


replaceFunctionInSourceFilesModuleText : AppFiles -> { functionName : String } -> String -> Result String String
replaceFunctionInSourceFilesModuleText sourceFiles { functionName } moduleText =
    getDeclarationFromElmModuleTextAndFunctionName
        { functionName = functionName, moduleText = moduleText }
        |> Result.andThen (Maybe.map Ok >> Maybe.withDefault (Err ("Did not find the function '" ++ functionName ++ "'")))
        |> Result.andThen
            (\functionDeclaration ->
                case parseSourceFileFunctionName functionName of
                    Err error ->
                        Err ("Failed to parse function name: " ++ error)

                    Ok { filePathRepresentation, base64 } ->
                        case findFileWithPathMatchingRepresentationInFunctionName sourceFiles filePathRepresentation of
                            Err error ->
                                Err ("Failed to identify file for '" ++ filePathRepresentation ++ "': " ++ error)

                            Ok ( _, fileContent ) ->
                                let
                                    functionLines =
                                        moduleText |> getTextLinesFromRange (Elm.Syntax.Node.range functionDeclaration)

                                    fileContentAsBase64 =
                                        fileContent |> Base64.fromBytes |> Maybe.withDefault "Error encoding to base64"

                                    base64Expression =
                                        "\"" ++ fileContentAsBase64 ++ "\""

                                    fileExpression =
                                        if base64 then
                                            base64Expression

                                        else
                                            [ base64Expression
                                            , "|> Base64.toBytes"
                                            , "|> Maybe.withDefault (\"Failed to convert from base64\" |> Bytes.Encode.string |> Bytes.Encode.encode)"
                                            ]
                                                |> String.join "\n"

                                    newFunctionLines =
                                        List.take 2 functionLines ++ [ indentElmCodeLines 1 fileExpression ]
                                in
                                addOrUpdateFunctionInElmModuleText
                                    { functionName = functionName, mapFunctionLines = always newFunctionLines }
                                    moduleText
            )


getDeclarationFromElmModuleTextAndFunctionName : { moduleText : String, functionName : String } -> Result String (Maybe (Elm.Syntax.Node.Node Elm.Syntax.Expression.Function))
getDeclarationFromElmModuleTextAndFunctionName { moduleText, functionName } =
    moduleText
        |> parseAndMapElmModuleText
            (\parsedModule ->
                parsedModule.declarations
                    |> List.filterMap
                        (\declaration ->
                            case Elm.Syntax.Node.value declaration of
                                Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                    Just (Elm.Syntax.Node.Node (Elm.Syntax.Node.range declaration) functionDeclaration)

                                _ ->
                                    Nothing
                        )
                    |> List.filter
                        (Elm.Syntax.Node.value
                            >> .declaration
                            >> Elm.Syntax.Node.value
                            >> .name
                            >> Elm.Syntax.Node.value
                            >> (==) functionName
                        )
                    |> List.head
                    |> Ok
            )


getTextLinesFromRange : Elm.Syntax.Range.Range -> String -> List String
getTextLinesFromRange range text =
    let
        lines =
            String.lines text
    in
    lines
        |> List.take range.end.row
        |> List.drop (range.start.row - 1)
        |> List.reverse
        |> listMapFirstElement (String.left (range.end.column - 1))
        |> List.reverse
        |> listMapFirstElement (String.dropLeft (range.start.column - 1))


listMapFirstElement : (a -> a) -> List a -> List a
listMapFirstElement mapElement list =
    case list of
        firstElement :: followingElements ->
            mapElement firstElement :: followingElements

        _ ->
            list


indentElmCodeLines : Int -> String -> String
indentElmCodeLines level =
    String.lines
        >> List.map ((++) (String.repeat level "    "))
        >> String.join "\n"


findFileWithPathMatchingRepresentationInFunctionName : AppFiles -> String -> Result String ( List String, Bytes.Bytes )
findFileWithPathMatchingRepresentationInFunctionName sourceFiles pathPattern =
    let
        matchingFiles =
            sourceFiles
                |> Dict.toList
                |> List.filter (Tuple.first >> pathMatchesPatternFromFunctionName pathPattern)
    in
    case matchingFiles of
        [ matchingFile ] ->
            Ok matchingFile

        [] ->
            Err
                ("Did not find any source file with a path matching the representation '"
                    ++ pathPattern
                    ++ "'. Here is a list of the available files: "
                    ++ String.join ", " (List.map (String.join "/") (Dict.keys sourceFiles))
                )

        _ ->
            Err
                ("The file path representation '"
                    ++ pathPattern
                    ++ "' is not unique because it matches "
                    ++ String.fromInt (List.length matchingFiles)
                    ++ " of the source files: "
                    ++ String.join ", " (List.map (Tuple.first >> String.join "/") matchingFiles)
                )


addOrUpdateFunctionInElmModuleText : { functionName : String, mapFunctionLines : Maybe (List String) -> List String } -> String -> Result String String
addOrUpdateFunctionInElmModuleText { functionName, mapFunctionLines } moduleText =
    moduleText
        |> parseAndMapElmModuleText
            (\parsedModule ->
                case
                    parsedModule.declarations
                        |> List.filter
                            (\declaration ->
                                case Elm.Syntax.Node.value declaration of
                                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                        let
                                            candidateFunctionName =
                                                Elm.Syntax.Node.value
                                                    (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                        in
                                        functionName == candidateFunctionName

                                    _ ->
                                        False
                            )
                        |> List.head
                of
                    Nothing ->
                        Ok (String.join "\n\n\n" [ moduleText, String.join "\n" (mapFunctionLines Nothing) ])

                    Just declaration ->
                        let
                            declarationRange =
                                Elm.Syntax.Node.range declaration

                            originalFunctionLines =
                                getTextLinesFromRange declarationRange moduleText
                        in
                        Ok
                            (replaceRangeInText declarationRange
                                (String.join "\n" (mapFunctionLines (Just originalFunctionLines)))
                                moduleText
                            )
            )


replaceRangeInText : Elm.Syntax.Range.Range -> String -> String -> String
replaceRangeInText range rangeReplacement originalText =
    let
        originalTextLines =
            String.lines originalText

        startLines =
            originalTextLines |> List.take (range.start.row - 1)

        lineStart =
            originalTextLines
                |> List.drop (List.length startLines)
                |> List.head
                |> Maybe.withDefault ""
                |> String.left (range.start.column - 1)
    in
    (originalTextLines |> List.take (range.start.row - 1))
        ++ [ lineStart ++ rangeReplacement ]
        ++ (originalTextLines |> List.drop range.end.row)
        |> String.join "\n"


addImportInElmModuleText : List String -> String -> Result String String
addImportInElmModuleText importModuleName moduleText =
    moduleText
        |> parseAndMapElmModuleText
            (\parsedModule ->
                let
                    moduleTextLines =
                        String.lines moduleText

                    insertionRow =
                        parsedModule.imports
                            |> List.map (Elm.Syntax.Node.range >> .end >> .row)
                            |> List.maximum
                            |> Maybe.withDefault (Elm.Syntax.Node.range parsedModule.moduleDefinition).end.row

                    importStatement =
                        "import " ++ String.join "." importModuleName
                in
                (List.take insertionRow moduleTextLines
                    ++ [ importStatement ]
                    ++ List.drop insertionRow moduleTextLines
                )
                    |> String.join "\n"
                    |> Ok
            )


parseAndMapElmModuleText : (Elm.Syntax.File.File -> Result String ok) -> String -> Result String ok
parseAndMapElmModuleText mapDependingOnParsing moduleText =
    case parseElmModuleText moduleText of
        Err error ->
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText error)

        Ok parsedModule ->
            mapDependingOnParsing parsedModule


{-| Consider restricting the interface of `mapElmModuleWithNameIfExists` to not support arbitrary changes to app code but only addition of expose syntax.
-}
mapElmModuleWithNameIfExists : String -> (( AppFiles, String ) -> Result String ( AppFiles, String )) -> AppFiles -> Result String AppFiles
mapElmModuleWithNameIfExists elmModuleName tryMapModuleText appCode =
    let
        elmModuleFilePath =
            filePathFromElmModuleName elmModuleName
    in
    case Dict.get elmModuleFilePath appCode of
        Nothing ->
            Ok appCode

        Just elmModuleFile ->
            case stringFromFileContent elmModuleFile of
                Nothing ->
                    Err "Failed to decode file content as string"

                Just moduleText ->
                    case tryMapModuleText ( appCode, moduleText ) of
                        Err mapModuleTextError ->
                            Err ("Failed to map module text: " ++ mapModuleTextError)

                        Ok ( newAppCode, newModuleText ) ->
                            newAppCode
                                |> Dict.insert elmModuleFilePath (Bytes.Encode.encode (Bytes.Encode.string newModuleText))
                                |> Ok


parseSourceFileFunctionName : String -> Result String { filePathRepresentation : String, base64 : Bool }
parseSourceFileFunctionName functionName =
    parseFlagsAndPathPatternFromFunctionName sourceFileFunctionNameStart functionName
        |> Result.map
            (\( flags, filePathRepresentation ) ->
                { filePathRepresentation = filePathRepresentation
                , base64 = flags |> List.member "base64"
                }
            )


parseFlagsAndPathPatternFromFunctionName : String -> String -> Result String ( List String, String )
parseFlagsAndPathPatternFromFunctionName requiredPrefix functionName =
    if not (String.startsWith requiredPrefix functionName) then
        Err ("Did not start with expected prefix of '" ++ requiredPrefix ++ "'")

    else
        let
            partAfterPrefix =
                String.dropLeft (String.length requiredPrefix) functionName
        in
        case String.indices functionNameFlagsSeparator partAfterPrefix of
            [] ->
                Err ("Missing separator '" ++ functionNameFlagsSeparator ++ "'")

            separatorIndex :: _ ->
                let
                    flags =
                        String.slice 0 separatorIndex partAfterPrefix
                            |> String.split "__"
                            |> List.filter (String.length >> (<) 0)
                in
                Ok
                    ( flags
                    , String.dropLeft (separatorIndex + String.length functionNameFlagsSeparator) partAfterPrefix
                    )


pathMatchesPatternFromFunctionName : String -> List String -> Bool
pathMatchesPatternFromFunctionName pathPattern path =
    filePathRepresentationInFunctionName path == pathPattern


filePathRepresentationInFunctionName : List String -> String
filePathRepresentationInFunctionName =
    String.join "/"
        >> String.toList
        >> List.map
            (\char ->
                if Char.isAlphaNum char then
                    char

                else
                    '_'
            )
        >> String.fromList


filePathFromElmModuleName : String -> List String
filePathFromElmModuleName elmModuleName =
    case elmModuleName |> String.split "." |> List.reverse of
        [] ->
            []

        moduleLastName :: reversedDirectoryNames ->
            [ "src" ] ++ List.reverse ((moduleLastName ++ ".elm") :: reversedDirectoryNames)


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    Elm.Parser.parse >> Result.map (Elm.Processing.process Elm.Processing.init)


stringFromFileContent : Bytes.Bytes -> Maybe String
stringFromFileContent bytes =
    Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes


fileContentFromString : String -> Bytes.Bytes
fileContentFromString =
    Bytes.Encode.string >> Bytes.Encode.encode


parserDeadEndsToString : String -> List Parser.DeadEnd -> String
parserDeadEndsToString parsedText deadEnds =
    String.concat (List.intersperse "; " (List.map (parserDeadEndToString parsedText) deadEnds))


parserDeadEndToString : String -> Parser.DeadEnd -> String
parserDeadEndToString parsedText deadend =
    parserProblemToString deadend.problem
        ++ " at row "
        ++ String.fromInt deadend.row
        ++ ", col "
        ++ String.fromInt deadend.col
        ++ ": '"
        ++ (parsedText |> String.lines |> List.drop (deadend.row - 2) |> List.take 4 |> String.join "\n")
        ++ "'"


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


stringStartsWithLowercaseLetter : String -> Bool
stringStartsWithLowercaseLetter =
    String.toList >> List.head >> Maybe.map Char.isLower >> Maybe.withDefault False


listFoldlToAggregateResult : (a -> b -> Result e b) -> Result e b -> List a -> Result e b
listFoldlToAggregateResult getElementResult =
    List.foldl
        (\element previousAggregateResult ->
            previousAggregateResult |> Result.andThen (\previousAggregate -> getElementResult element previousAggregate)
        )
