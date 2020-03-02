using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Kalmit
{
    public class CompileElmValueSerializer
    {
        static string encodeParamName => "valueToEncode";

        static string jsonEncodeFunctionNamePrefix => "jsonEncode_";

        static string jsonDecodeFunctionNamePrefix => "jsonDecode_";

        static string jsonCodeMaybeFunctionNameCommonPart => "_generic_Maybe";

        static string jsonCodeListFunctionNameCommonPart => "_generic_List";

        static string jsonCodeSetFunctionNameCommonPart => "_generic_Set";

        static string jsonCodeDictFunctionNameCommonPart => "_generic_Dict";

        static string jsonCodeResultFunctionNameCommonPart => "_generic_Result";

        static string jsonCodeTupleFunctionNameCommonPart => "_tuple_";

        static IImmutableDictionary<string, (string encodeExpression, string decodeExpression)> LeafExpressions =>
            ImmutableDictionary<string, (string encodeExpression, string decodeExpression)>.Empty
            .Add("String", ("Json.Encode.string " + encodeParamName, "Json.Decode.string"))
            .Add("Int", ("Json.Encode.int " + encodeParamName, "Json.Decode.int"))
            .Add("Bool", ("Json.Encode.bool " + encodeParamName, "Json.Decode.bool"))
            .Add("Float", ("Json.Encode.float " + encodeParamName, "Json.Decode.float"))
            .Add("()", ("Json.Encode.list (always (Json.Encode.object [])) []", "Json.Decode.succeed ()"))
            .Add("{}", ("Json.Encode.object []", "Json.Decode.succeed {}"));

        static IImmutableDictionary<string, string> InstantiationSpecialCases =>
            ImmutableDictionary<string, string>.Empty
            .Add("List", jsonCodeListFunctionNameCommonPart)
            .Add("Set.Set", jsonCodeSetFunctionNameCommonPart)
            .Add("Maybe", jsonCodeMaybeFunctionNameCommonPart)
            .Add("Result", jsonCodeResultFunctionNameCommonPart)
            .Add("Dict.Dict", jsonCodeDictFunctionNameCommonPart);

        public struct ResolveTypeResult
        {
            public string canonicalTypeText;

            public string canonicalTypeTextWithParameters;

            public Func<(string encodeExpression, string decodeExpression, IImmutableSet<string> dependencies)> compileExpressions;

            public IImmutableSet<string> referencedModules;
        }

        static bool StartsWithLowercaseLetter(string text) =>
            text.Length < 1 ? false : Char.IsLower(text[0]);

        static public ((string encodeFunctionName, string decodeFunctionName, string commonPart) functionNames,
            IImmutableList<string> typeParameterNames)
            GetFunctionNamesAndTypeParametersFromTypeText(string typeText)
        {
            var (typeTextMinusTypeParameters, typeParametersNames) = parseForTypeParameters(typeText);

            var rootTypeTextHash = CommonConversion.HashSHA256(Encoding.UTF8.GetBytes(typeTextMinusTypeParameters));

            var functionNameCommonPart =
                StartsWithLowercaseLetter(typeTextMinusTypeParameters.Trim())
                ?
                "type_parameter_" + typeTextMinusTypeParameters
                :
                (Regex.IsMatch(typeTextMinusTypeParameters, @"^[\w\d_\.]+$") ?
                "named_" + typeTextMinusTypeParameters.Replace(".", "_dot_")
                :
                "anonymous_" + CommonConversion.StringBase16FromByteArray(rootTypeTextHash).Substring(0, 10));

            return
                ((jsonEncodeFunctionNamePrefix + functionNameCommonPart,
                jsonDecodeFunctionNamePrefix + functionNameCommonPart,
                functionNameCommonPart), typeParametersNames);
        }

        static public (string encodeFunction, string decodeFunction) BuildJsonCodingFunctionTexts(
            string typeText,
            string encodeExpression,
            string decodeExpression)
        {
            var (functionNames, typeParametersNames) = GetFunctionNamesAndTypeParametersFromTypeText(typeText);

            var typeParameters =
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
                .ToImmutableList();

            string annotationsFromTypeList(IEnumerable<string> types) =>
                string.Join(" -> ", types.Select(@type => "(" + @type + ")"));

            var encodeFunction =
                functionNames.encodeFunctionName + " : " +
                annotationsFromTypeList(
                    typeParameters.Select(typeParam => typeParam.encodeAnnotation).Concat(
                        new[] { typeText, "Json.Encode.Value" })) + "\n" +
                functionNames.encodeFunctionName + " " +
                string.Join(" ", typeParameters.Select(typeParam => typeParam.encodeParameter)) +
                " " + encodeParamName + " =\n" +
                IndentElmCodeLines(1, encodeExpression);

            var decodeFunction =
                functionNames.decodeFunctionName + " : " +
                annotationsFromTypeList(
                    typeParameters.Select(typeParam => typeParam.decodeAnnotation).Concat(
                        new[] { "Json.Decode.Decoder (" + typeText + ")" })) + "\n" +
                functionNames.decodeFunctionName + " " +
                string.Join(" ", typeParameters.Select(typeParam => typeParam.decodeParameter)) +
                " =\n" +
                IndentElmCodeLines(1, decodeExpression);

            return (encodeFunction, decodeFunction);
        }

        static (string typeTextMinusTypeParameters, ImmutableList<string> typeParametersNames) parseForTypeParameters(string typeText)
        {
            var instanceTypeTextWithParametersMatch = Regex.Match(typeText.Trim(), @"^\((.+?)((\s+[a-z][^\s]*){1,})\)$");

            if (instanceTypeTextWithParametersMatch.Success)
                return
                (instanceTypeTextWithParametersMatch.Groups[1].Value,
                Regex.Split(instanceTypeTextWithParametersMatch.Groups[2].Value.Trim(), @"\s+").ToImmutableList());

            var parseTypeResult = ParseElmTypeText(typeText);

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

        static IEnumerable<string> EnumerateAllTypeNamesFromTypeText(string typeText)
        {
            var parseTypeResult = ParseElmTypeText(typeText);

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

        static public ResolveTypeResult ResolveType(
            string rootTypeText,
            string sourceModuleName,
            Func<string, string> getModuleText,
            Action<string> logWriteLine)
        {
            rootTypeText = rootTypeText.Trim();

            logWriteLine?.Invoke("Begin ResolveType for '" + rootTypeText + "' in module '" + sourceModuleName + "'.");

            var sourceModuleText = getModuleText(sourceModuleName);

            if (LeafExpressions.TryGetValue(rootTypeText, out var leafExpressions))
            {
                logWriteLine?.Invoke("Found a leaf for type '" + rootTypeText + "'.");

                return new ResolveTypeResult
                {
                    canonicalTypeText = rootTypeText,

                    compileExpressions = () => (leafExpressions.encodeExpression, leafExpressions.decodeExpression, ImmutableHashSet<string>.Empty),

                    referencedModules = ImmutableHashSet<string>.Empty,
                };
            }

            (string referencedModuleName, string typeNameInReferencedModule)
                GetCanonicalModuleNameAndLocalTypeNameFromNameInSourceModule(string nameInThisModule)
            {
                var importedName = GetCanonicalNameFromImportedNameInModule(nameInThisModule, sourceModuleText);

                if (importedName != null)
                    return importedName.Value;

                if (nameInThisModule.Contains('.'))
                    throw new Exception("Failed to look up name '" + nameInThisModule + "'.");

                return (sourceModuleName, nameInThisModule);
            }

            (string canonicalTypeText, IImmutableSet<string> dependencies) ResolveLocalTypeText(string typeText)
            {
                if (StartsWithLowercaseLetter(typeText))
                    return (typeText, ImmutableHashSet<string>.Empty);

                var canonicalTypeText = ResolveType(typeText, sourceModuleName, getModuleText, logWriteLine).canonicalTypeText;

                return (canonicalTypeText, ImmutableHashSet.Create(canonicalTypeText));
            }

            var rootType = ParseElmTypeText(rootTypeText).parsedType;

            if (rootType.Alias != null)
            {
                logWriteLine?.Invoke("Type '" + rootTypeText + "' is alias for '" + rootType.Alias.Value.aliasedText + "'.");

                return ResolveType(
                    rootType.Alias.Value.aliasedText,
                    sourceModuleName,
                    getModuleText,
                    logWriteLine);
            }

            if (rootType.Instance != null)
            {
                if (rootType.Instance.Value.parameters.Count == 0)
                {
                    var (referencedModuleName, typeNameInReferencedModule) =
                        GetCanonicalModuleNameAndLocalTypeNameFromNameInSourceModule(rootTypeText);

                    if (referencedModuleName != sourceModuleName)
                    {
                        logWriteLine?.Invoke("Type '" + rootTypeText + "' in '" + sourceModuleName + "' refers to '" + referencedModuleName + "." + typeNameInReferencedModule + "'.");

                        return ResolveType(
                            typeNameInReferencedModule,
                            referencedModuleName,
                            getModuleText,
                            logWriteLine);
                    }

                    logWriteLine?.Invoke("Resolve type text for '" + rootTypeText + "' in '" + sourceModuleName + "'.");

                    var referencedTypeText = GetTypeDefinitionTextFromModuleText(rootTypeText, sourceModuleText);

                    if (!(0 < referencedTypeText?.Length))
                        throw new Exception("Did not find the definition of type '" + rootTypeText + "'.");

                    return ResolveType(referencedTypeText, sourceModuleName, getModuleText, logWriteLine);
                }

                logWriteLine?.Invoke("Type '" + rootTypeText + "' is instance of '" +
                    rootType.Instance.Value.typeName + "' with " +
                    rootType.Instance.Value.parameters.Count + " parameters.");

                var parameters =
                    rootType.Instance.Value.parameters
                    .Select(parameter =>
                    {
                        var dependencies = ResolveType(parameter, sourceModuleName, getModuleText, logWriteLine);

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
                    var instantiatedTypeResolution = ResolveType(rootType.Instance.Value.typeName, sourceModuleName, getModuleText, logWriteLine);
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
            }

            if (rootType.Record != null)
            {
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
            }

            if (rootType.Custom != null)
            {
                logWriteLine?.Invoke("'" + rootTypeText + "' is a custom type.");

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
                                    encodeCase = encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax("", "Json.Encode.object []"),
                                    decodeExpression = decodeSyntaxCommonPart + " (Json.Decode.succeed " + typeTagCanonicalName + ")",
                                    dependencies = ImmutableHashSet<string>.Empty,
                                };
                            }

                            if (1 < typeTag.Value.Count)
                                throw new NotSupportedException("Problem with '" + typeTagCanonicalName + "': Tags with more than one parameter are not supported.");

                            var tagParameterType = typeTag.Value.Single();

                            var tagParameterCanonicalTypeTextAndDependencies = ResolveLocalTypeText(tagParameterType);

                            var (tagParameterTypeFunctionNames, tagParameterTypeParametersNames) =
                                GetFunctionNamesAndTypeParametersFromTypeText(tagParameterCanonicalTypeTextAndDependencies.canonicalTypeText);

                            var tagParameterEncodeFunction =
                                expressionTextForFunctionWithOptionalTypeParameters(
                                    tagParameterTypeFunctionNames.encodeFunctionName, jsonEncodeFunctionNamePrefix, tagParameterTypeParametersNames);

                            var tagParameterDecodeFunction =
                                expressionTextForFunctionWithOptionalTypeParameters(
                                    tagParameterTypeFunctionNames.decodeFunctionName, jsonDecodeFunctionNamePrefix, tagParameterTypeParametersNames);

                            var argumentName = "tagArgument";

                            var tagDecodeExpression =
                                decodeSyntaxCommonPart + @" (Json.Decode.lazy (\_ -> " +
                                tagParameterDecodeFunction + " |> Json.Decode.map " + typeTagCanonicalName +
                                " ) )";

                            return new
                            {
                                encodeCase = encodeCaseSyntaxFromArgumentSyntaxAndObjectContentSyntax(
                                    argumentName,
                                    argumentName + " |> " + tagParameterEncodeFunction),
                                decodeExpression = tagDecodeExpression,
                                dependencies = tagParameterCanonicalTypeTextAndDependencies.dependencies.ToImmutableHashSet(),
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
            }

            if (rootType.Tuple != null)
            {
                logWriteLine?.Invoke("'" + rootTypeText + "' is a " + rootType.Tuple.Count + "-tuple.");

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
            }

            throw new Exception("Parsed invalid case for type '" + rootTypeText + "'");
        }

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


        static string ElmCodeIndentString(int level) =>
            level <= 0 ? "" : "    " + ElmCodeIndentString(level - 1);

        static public string IndentElmCodeLines(int level, string textBeforeIndent)
        {
            var indentString = ElmCodeIndentString(level);

            return indentString + textBeforeIndent.Replace("\n", "\n" + indentString);
        }

        static public (ElmType parsedType, string remainingString) ParseElmTypeText(
            string elmTypeText)
        {
            //  Assume: Custom type tags are all on their own line.

            try
            {
                var typeDefinitionTextLines = elmTypeText.Split(new char[] { '\n' });

                var aliasMatch = Regex.Match(typeDefinitionTextLines[0], @"^type\s+alias\s+([A-Z][^\s]*(\s+[a-z][^\s]*){0,})\s*=");

                if (aliasMatch.Success)
                {
                    var aliasNameAndParameters = Regex.Split(aliasMatch.Groups[1].Value.Trim(), @"\s+");

                    var aliasLocalName = aliasNameAndParameters.First();
                    var parameters = aliasNameAndParameters.Skip(1).ToImmutableList();

                    return (parsedType: new ElmType
                    {
                        Alias = new ElmType.AliasStructure
                        {
                            aliasLocalName = aliasLocalName,
                            parameters = parameters,
                            aliasedText = string.Join("\n", typeDefinitionTextLines.Skip(1).ToArray()),
                        },
                    }, "");
                }

                var customTypeMatch = Regex.Match(string.Join("", typeDefinitionTextLines), @"^type\s+([A-Z][^\s]*(\s+[a-z][^\s]*){0,})\s*=");

                if (customTypeMatch.Success)
                {
                    var typeNameAndParameters = Regex.Split(customTypeMatch.Groups[1].Value, @"\s+");

                    var typeLocalName = typeNameAndParameters.First();
                    var parameters = typeNameAndParameters.Skip(1).ToImmutableList();

                    var tags =
                        typeDefinitionTextLines.Skip(1)
                        .Select(tagLine =>
                        {
                            var overallMatch = Regex.Match(tagLine, @"^\s+(=|\|)\s*([\w\d_]+)(.*)$");

                            if (!overallMatch.Success)
                                throw new Exception("Failed to parse Custom Type tag name from '" + tagLine + "'");

                            var parametersText = overallMatch.Groups[3].Value.Trim();

                            var tagParametersTexts = new List<string>();

                            var tagParametersTextRemaining = parametersText;

                            while (0 < tagParametersTextRemaining?.Length)
                            {
                                var (parameter, remainingAfterParameter) = ParseElmTypeText(tagParametersTextRemaining);

                                var tagParameterText =
                                    tagParametersTextRemaining
                                    .Substring(0, tagParametersTextRemaining.Length - remainingAfterParameter.Length)
                                    .Trim();

                                tagParametersTexts.Add(tagParameterText);

                                tagParametersTextRemaining = remainingAfterParameter.TrimStart();
                            }

                            return (tagName: overallMatch.Groups[2].Value, tagParameters: (IImmutableList<string>)tagParametersTexts.ToImmutableList());
                        })
                    .ToImmutableDictionary(tag => tag.tagName, tag => tag.tagParameters);

                    return (parsedType: new ElmType
                    {
                        Custom = new ElmType.CustomStructure
                        {
                            typeLocalName = typeLocalName,
                            parameters = parameters,
                            tags = tags
                        },
                    }, "");
                }

                var withoutLeadingWhitespace = elmTypeText.TrimStart();

                switch (withoutLeadingWhitespace[0])
                {
                    case '(':
                        var parenthesesRest = withoutLeadingWhitespace.Substring(1);

                        var elements = new List<string>();

                        while (true)
                        {
                            var (_, restWithoutLeadingWhitespace) = parseRegexPattern(parenthesesRest, @"\s*");

                            if (restWithoutLeadingWhitespace.Length < 1)
                                throw new Exception("Missing terminating parenthesis.");

                            var (termination, restAfterTermination) = parseRegexPattern(restWithoutLeadingWhitespace, @"\)");

                            if (0 < termination.Length)
                            {
                                var parsedType =
                                    1 == elements.Count ?
                                    ParseElmTypeText(elements.Single()).parsedType
                                    :
                                    new ElmType
                                    {
                                        Tuple = elements.ToImmutableList()
                                    };

                                return (parsedType, restAfterTermination);
                            }

                            if (0 < elements.Count)
                            {
                                var (separator, restAfterSeparator) = parseRegexPattern(restWithoutLeadingWhitespace, @",\s*");

                                if (!(0 < separator.Length))
                                    throw new Exception("Missing separator.");

                                restWithoutLeadingWhitespace = restAfterSeparator.TrimStart();
                            }

                            var parsedElement = ParseElmTypeText(restWithoutLeadingWhitespace);

                            var parsedElementText = restWithoutLeadingWhitespace.Substring(0, restWithoutLeadingWhitespace.Length - parsedElement.remainingString.Length);

                            elements.Add(parsedElementText);
                            parenthesesRest = parsedElement.remainingString;
                        }

                    case '{':
                        var recordRest = withoutLeadingWhitespace.Substring(1);

                        var fields = new List<(string fieldName, string fieldTypeText, ElmType fieldType)>();

                        while (true)
                        {
                            var recordRestWithoutLeadingWhitespace = recordRest.TrimStart();

                            if (recordRestWithoutLeadingWhitespace[0] == '}')
                            {
                                return (parsedType: new ElmType
                                {
                                    Record = new ElmType.RecordStructure
                                    {
                                        fields = fields.ToImmutableList(),
                                    }
                                }, remainingString: recordRestWithoutLeadingWhitespace.Substring(1));
                            }

                            var (_, restAfterFieldSeparator) = parseRegexPattern(recordRestWithoutLeadingWhitespace, @"\s*,\s*");

                            var (fieldName, restAfterFieldName) = parseFieldName(restAfterFieldSeparator);

                            var (_, restAfterFieldColon) = parseRegexPattern(restAfterFieldName, @"\s*:\s*");

                            var (fieldType, restAfterFieldType) = ParseElmTypeText(restAfterFieldColon);

                            recordRest = restAfterFieldType;

                            var fieldTypeTextLength = restAfterFieldColon.Length - recordRest.Length;

                            if (fieldName.Length < 1)
                                throw new Exception("Missing termination token.");

                            fields.Add((fieldName, fieldTypeText: restAfterFieldColon.Substring(0, fieldTypeTextLength), fieldType));
                        }

                    case char firstCharacter when Char.IsLetter(firstCharacter) || firstCharacter == '_':

                        var nameInInstanceRegexPattern = @"[\w\d_\.]+";

                        var (firstName, restAfterFirstName) = parseRegexPattern(withoutLeadingWhitespace, nameInInstanceRegexPattern);

                        var parametersTexts = new List<string>();

                        var instanceRest = restAfterFirstName;

                        while (true)
                        {
                            var (_, instanceRestWithoutWhitespace) = parseRegexPattern(instanceRest, @"\s*");

                            var (parameterName, restAfterParameterName) = parseRegexPattern(
                                instanceRestWithoutWhitespace, nameInInstanceRegexPattern);

                            if (0 < parameterName.Length)
                            {
                                parametersTexts.Add(parameterName);
                                instanceRest = restAfterParameterName;
                                continue;
                            }

                            var (parameterTypeBegin, _) = parseRegexPattern(instanceRestWithoutWhitespace, @"[\(\{]");

                            if (0 < parameterTypeBegin.Length)
                            {
                                var parameterParsed = ParseElmTypeText(instanceRestWithoutWhitespace);

                                var parameterTypeText = instanceRestWithoutWhitespace.Substring(0, instanceRestWithoutWhitespace.Length - parameterParsed.remainingString.Length);

                                parametersTexts.Add(parameterTypeText);
                                instanceRest = parameterParsed.remainingString;
                                continue;
                            }

                            return (parsedType: new ElmType
                            {
                                Instance = (typeName: firstName, parametersTexts.ToImmutableList()),
                            },
                            remainingString: instanceRest);
                        }

                    case char other:
                        throw new NotSupportedException("Unexpected first character in type text: '" + other.ToString() + "'.");
                }

                throw new NotImplementedException("Type text did not match any supported pattern.");
            }
            catch (Exception e)
            {
                throw new Exception("Failed to parse type '" + elmTypeText + "'.", e);
            }
        }

        static (string fieldName, string remainingString) parseFieldName(string originalString)
        {
            return parseRegexPattern(originalString, @"[\w\d_]+");
        }

        static (string matchValue, string remainingString) parseRegexPattern(string originalString, string regexPattern)
        {
            var match = Regex.Match(originalString, "^" + regexPattern);

            return (matchValue: match.Value, remainingString: originalString.Substring(match.Length));
        }

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

        static public IImmutableList<string> GetElmModuleTextLines(string moduleText) =>
            moduleText.Replace("\r", "").Split(new[] { '\n' }, StringSplitOptions.None)
            .ToImmutableList();

        static public string GetTypeDefinitionTextFromModuleText(string typeNameInModule, string moduleText)
        {
            //  Assume: There are no empty lines and no comments in the type definition.

            var match =
                Regex.Match(
                    moduleText.Replace("\r", ""),
                    @"(?<=(^|\n))type\s+(|alias\s+)" + typeNameInModule + @"\s+.+?(?=\n($|\n))",
                    RegexOptions.Singleline);

            if (!match.Success)
                return null;

            return match.Value;
        }

        public struct ElmType
        {
            public CustomStructure? Custom;

            public RecordStructure? Record;

            public AliasStructure? Alias;

            public (string typeName, IImmutableList<string> parameters)? Instance;

            public IImmutableList<string> Tuple;

            public struct AliasStructure
            {
                public string aliasLocalName;

                public IImmutableList<string> parameters;

                public string aliasedText;
            }

            public struct CustomStructure
            {
                public string typeLocalName;

                public IImmutableList<string> parameters;

                public IImmutableDictionary<string, IImmutableList<string>> tags;
            }

            public struct RecordStructure
            {
                public IImmutableList<(string name, string typeText, ElmType parsedType)> fields;
            }
        }

        static public IEnumerable<(string elmType, (string encodeExpression, string decodeExpression, IImmutableSet<string> referencedModules) result)>
            EnumerateExpressionsResolvingAllDependencies(
                Func<string, ResolveTypeResult> getExpressionsAndDependenciesForType,
                IImmutableSet<string> rootTypes)
        {
            var remainingDependencies = new Queue<string>(rootTypes);

            var alreadyResolved = new HashSet<string>();

            while (0 < remainingDependencies.Count)
            {
                var currentType = remainingDependencies.Dequeue();

                if (alreadyResolved.Contains(currentType))
                    continue;

                alreadyResolved.Add(currentType);

                var currentTypeResults = getExpressionsAndDependenciesForType(currentType);

                var currentTypeExpressions = currentTypeResults.compileExpressions();

                foreach (var dependency in currentTypeExpressions.dependencies)
                    remainingDependencies.Enqueue(dependency);

                yield return
                    (currentTypeResults.canonicalTypeTextWithParameters ?? currentTypeResults.canonicalTypeText,
                    (currentTypeExpressions.encodeExpression, currentTypeExpressions.decodeExpression, currentTypeResults.referencedModules));
            }
        }

        static public IImmutableList<string> generalSupportingFunctionsTexts => new[]{
            jsonEncodeFunctionNamePrefix + jsonCodeMaybeFunctionNameCommonPart + $@" encoder valueToEncode =
    case valueToEncode of
        Nothing ->
            [ ( ""Nothing"", [] |> Json.Encode.object ) ] |> Json.Encode.object

        Just just ->
            [ ( ""Just"", just |> encoder ) ] |> Json.Encode.object
",
            jsonDecodeFunctionNamePrefix + jsonCodeMaybeFunctionNameCommonPart + $@" decoder =
    Json.Decode.oneOf
        [ Json.Decode.field ""Nothing"" (Json.Decode.succeed Nothing)
        , Json.Decode.field ""Just"" (decoder |> Json.Decode.map Just)
        , Json.Decode.null Nothing -- Temporary backwardscompatibility: Map 'null' to Nothing
        ]
",

            jsonEncodeFunctionNamePrefix + jsonCodeListFunctionNameCommonPart + $@" encoder =
    Json.Encode.list encoder
",
            jsonDecodeFunctionNamePrefix + jsonCodeListFunctionNameCommonPart + $@" decoder =
    Json.Decode.list decoder
",

            jsonEncodeFunctionNamePrefix + jsonCodeSetFunctionNameCommonPart + $@" encoder =
    Set.toList >> Json.Encode.list encoder
",
            jsonDecodeFunctionNamePrefix + jsonCodeSetFunctionNameCommonPart + $@" decoder =
    Json.Decode.list decoder |> Json.Decode.map Set.fromList
",
            jsonEncodeFunctionNamePrefix + jsonCodeDictFunctionNameCommonPart + $@" encodeKey encodeValue =
    Dict.toList >> Json.Encode.list (" + jsonEncodeFunctionNamePrefix + jsonCodeTupleFunctionNameCommonPart + "2 encodeKey encodeValue)",

            jsonDecodeFunctionNamePrefix + jsonCodeDictFunctionNameCommonPart + $@" decodeKey decodeValue =
    let
        -- Support migrate applications automatically from older framework versions:

        oldElementDecoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.field ""key"" decodeKey)
                (Json.Decode.field ""value"" decodeValue)
    in
    Json.Decode.list (Json.Decode.oneOf [ " + jsonDecodeFunctionNamePrefix + jsonCodeTupleFunctionNameCommonPart + $@"2 decodeKey decodeValue, oldElementDecoder ])
        |> Json.Decode.map Dict.fromList
",
            jsonEncodeFunctionNamePrefix + jsonCodeResultFunctionNameCommonPart + $@" encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( ""Err"", valueToEncodeError |> encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( ""Ok"", valueToEncodeOk |> encodeOk ) ] |> Json.Encode.object
",
            jsonDecodeFunctionNamePrefix + jsonCodeResultFunctionNameCommonPart + $@" decodeErr decodeOk =
    Json.Decode.oneOf
        [ Json.Decode.field ""Err"" decodeErr |> Json.Decode.map Err
        , Json.Decode.field ""Ok"" decodeOk |> Json.Decode.map Ok
        ]
",
            jsonEncodeFunctionNamePrefix + jsonCodeTupleFunctionNameCommonPart + $@"2 encodeA encodeB ( a, b ) =
    [ a |> encodeA, b |> encodeB ]
        |> Json.Encode.list identity
",
            jsonDecodeFunctionNamePrefix + jsonCodeTupleFunctionNameCommonPart + $@"2 decodeA decodeB =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)
",
            jsonEncodeFunctionNamePrefix + jsonCodeTupleFunctionNameCommonPart + $@"3 encodeA encodeB encodeC ( a, b, c ) =
    [ a |> encodeA, b |> encodeB, c |> encodeC ]
        |> Json.Encode.list identity
",
            jsonDecodeFunctionNamePrefix + jsonCodeTupleFunctionNameCommonPart + $@"3 decodeA decodeB decodeC =
    Json.Decode.map3 (\a b c -> ( a, b, c ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)
        (Json.Decode.index 2 decodeC)
",
            $@"{{-| As found at <https://github.com/elm-community/json-extra/blob/14b45543fb85531385eb9ac9adca2c054f73e624/src/Json/Decode/Extra.elm#L144-L146>
-}}
jsonDecode_andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
jsonDecode_andMap =
    Json.Decode.map2 (|>)
"
        }.ToImmutableList();
    }
}