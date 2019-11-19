using System;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Kalmit
{
    class CompileElmValueSerializer
    {
        static string encodeParamName => "valueToEncode";

        static string jsonEncodeFunctionNamePrefix => "jsonEncode_";

        static string jsonDecodeFunctionNamePrefix => "jsonDecode_";

        static string jsonCodeMaybeFunctionNameCommonPart => "_generic_Maybe";

        static string jsonCodeListFunctionNameCommonPart => "_generic_List";

        static string jsonCodeDictFunctionNameCommonPart => "_generic_Dict";

        static string jsonCodeResultFunctionNameCommonPart => "_generic_Result";

        static ImmutableDictionary<string, (string encodeExpression, string decodeExpression)> LeafExpressions =>
            ImmutableDictionary<string, (string encodeExpression, string decodeExpression)>.Empty
            .Add("String", ("Json.Encode.string " + encodeParamName, "Json.Decode.string"))
            .Add("Int", ("Json.Encode.int " + encodeParamName, "Json.Decode.int"))
            .Add("Bool", ("Json.Encode.bool " + encodeParamName, "Json.Decode.bool"))
            .Add("Float", ("Json.Encode.float " + encodeParamName, "Json.Decode.float"))
            .Add("()", ("Json.Encode.object []", "Json.Decode.succeed ()"))
            .Add("{}", ("Json.Encode.object []", "Json.Decode.succeed {}"));

        public struct CompileSerializingExpressionsResult
        {
            public string canonicalTypeText;

            public string encodeExpression;

            public string decodeExpression;

            public IImmutableDictionary<string, CompileSerializingExpressionsResult> dependencies;

            public IImmutableSet<string> referencedModules;
        }

        static public (string encodeFunctionName, string decodeFunctionName) GetFunctionNamesFromTypeText(
            string typeText)
        {
            var rootTypeTextHash = CommonConversion.HashSHA256(Encoding.UTF8.GetBytes(typeText));

            var functionNameCommonPart =
                Regex.IsMatch(typeText, @"^[\w\d_\.]+$") ?
                "named_" + typeText.Replace(".", "_dot_")
                :
                "anonymous_" + CommonConversion.StringBase16FromByteArray(rootTypeTextHash).Substring(0, 10);

            return
                (jsonEncodeFunctionNamePrefix + functionNameCommonPart,
                jsonDecodeFunctionNamePrefix + functionNameCommonPart);
        }

        static public (string encodeFunction, string decodeFunction) BuildFunctionTextsFromExpressions(
            string typeText,
            string encodeExpression,
            string decodeExpression)
        {
            var functionNames = GetFunctionNamesFromTypeText(typeText);

            var encodeFunction =
                functionNames.encodeFunctionName + " : " + typeText + " -> Json.Encode.Value\n" +
                functionNames.encodeFunctionName + " " + encodeParamName + " =\n" +
                IndentElmCodeLines(1, encodeExpression);

            var decodeFunction =
                functionNames.decodeFunctionName + " : Json.Decode.Decoder (" + typeText + ")\n" +
                functionNames.decodeFunctionName + " =\n" +
                IndentElmCodeLines(1, decodeExpression);

            return
                (encodeFunction, decodeFunction);
        }

        static public CompileSerializingExpressionsResult CompileSerializingExpressions(
            string rootTypeText,
            string sourceModuleName,
            Func<string, string> getModuleText)
        {
            Console.WriteLine("Begin BuildJsonCodingExpressions for '" + rootTypeText + "' in module '" + sourceModuleName + "'.");

            var sourceModuleText = getModuleText(sourceModuleName);

            if (LeafExpressions.TryGetValue(rootTypeText, out var leafExpressions))
            {
                Console.WriteLine("Found a leaf for type '" + rootTypeText + "'.");

                var functionNames = GetFunctionNamesFromTypeText(rootTypeText);

                return new CompileSerializingExpressionsResult
                {
                    canonicalTypeText = rootTypeText,

                    encodeExpression = leafExpressions.encodeExpression,
                    decodeExpression = leafExpressions.decodeExpression,

                    dependencies = ImmutableDictionary<string, CompileSerializingExpressionsResult>.Empty,
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

            if (!Regex.IsMatch(rootTypeText, @"\s"))
            {
                //  If the type text does not contain any whitespaces, assume it is a name.

                //  Look what this name references to.

                var (referencedModuleName, typeNameInReferencedModule) =
                    GetCanonicalModuleNameAndLocalTypeNameFromNameInSourceModule(rootTypeText);

                if (referencedModuleName != sourceModuleName)
                {
                    Console.WriteLine("Type '" + rootTypeText + "' in '" + sourceModuleName + "' refers to '" + referencedModuleName + "." + typeNameInReferencedModule + "'.");

                    return CompileSerializingExpressions(
                        typeNameInReferencedModule,
                        referencedModuleName,
                        getModuleText);
                }

                Console.WriteLine("Resolve type text for '" + rootTypeText + "' in '" + sourceModuleName + "'.");

                var referencedTypeText = GetTypeDefinitionTextFromModuleText(rootTypeText, sourceModuleText);

                if (!(0 < referencedTypeText?.Length))
                    throw new Exception("Did not find the definition of type '" + rootTypeText + "'.");

                var supportingExpressions =
                    CompileSerializingExpressions(
                        referencedTypeText,
                        sourceModuleName,
                        getModuleText);

                var fullyQualifiedTypeName = sourceModuleName + "." + rootTypeText;

                return
                    new CompileSerializingExpressionsResult
                    {
                        canonicalTypeText = fullyQualifiedTypeName,
                        encodeExpression = supportingExpressions.encodeExpression,
                        decodeExpression = supportingExpressions.decodeExpression,
                        dependencies = supportingExpressions.dependencies,
                        referencedModules = supportingExpressions.referencedModules.Add(referencedModuleName),
                    };
            }

            var rootType = ParseElmTypeText(rootTypeText);

            if (rootType.Alias != null)
            {
                Console.WriteLine("Type '" + rootTypeText + "' is alias for '" + rootType.Alias + "'.");

                return CompileSerializingExpressions(
                    rootType.Alias,
                    sourceModuleName,
                    getModuleText);
            }

            if (rootType.Instance != null)
            {
                Console.WriteLine("Type '" + rootTypeText + "' is instance of '" + rootType.Instance.Value.genericType + "'.");

                var parameters =
                    rootType.Instance.Value.parameters
                    .Select(parameter =>
                    {
                        var dependencies = CompileSerializingExpressions(parameter, sourceModuleName, getModuleText);

                        var functionNames = GetFunctionNamesFromTypeText(dependencies.canonicalTypeText);

                        return new { functionNames, dependencies };
                    })
                    .ToImmutableList();

                //  TODO: Consolidate 'Maybe' and 'List'

                if (rootType.Instance.Value.genericType == "Maybe")
                {
                    return new CompileSerializingExpressionsResult
                    {
                        canonicalTypeText = "Maybe " + parameters.Single().dependencies.canonicalTypeText,

                        encodeExpression = jsonEncodeFunctionNamePrefix + jsonCodeMaybeFunctionNameCommonPart + " " + parameters.Single().functionNames.encodeFunctionName + " " + encodeParamName,
                        decodeExpression = jsonDecodeFunctionNamePrefix + jsonCodeMaybeFunctionNameCommonPart + " " + parameters.Single().functionNames.decodeFunctionName,

                        dependencies = parameters.ToImmutableDictionary(
                            parameter => parameter.dependencies.canonicalTypeText,
                            parameter => parameter.dependencies),

                        referencedModules = parameters.SelectMany(parameter => parameter.dependencies.referencedModules).ToImmutableHashSet()
                    };
                }

                if (rootType.Instance.Value.genericType == "List")
                {
                    return new CompileSerializingExpressionsResult
                    {
                        canonicalTypeText = "List " + parameters.Single().dependencies.canonicalTypeText,

                        encodeExpression = jsonEncodeFunctionNamePrefix + jsonCodeListFunctionNameCommonPart + " " + parameters.Single().functionNames.encodeFunctionName + " " + encodeParamName,
                        decodeExpression = jsonDecodeFunctionNamePrefix + jsonCodeListFunctionNameCommonPart + " " + parameters.Single().functionNames.decodeFunctionName,

                        dependencies = parameters.ToImmutableDictionary(
                            parameter => parameter.dependencies.canonicalTypeText,
                            parameter => parameter.dependencies),

                        referencedModules = parameters.SelectMany(parameter => parameter.dependencies.referencedModules).ToImmutableHashSet()
                    };
                }

                if (rootType.Instance.Value.genericType == "Dict.Dict")
                {
                    if (parameters.Count != 2)
                        throw new Exception("Unexpected number of parameters for 'Dict.Dict': got " + parameters.Count + " instead of 2.");

                    return new CompileSerializingExpressionsResult
                    {
                        canonicalTypeText = "Dict.Dict " + String.Join(" ", parameters.Select(param => param.dependencies.canonicalTypeText)),

                        encodeExpression = jsonEncodeFunctionNamePrefix + jsonCodeDictFunctionNameCommonPart + " " + String.Join(" ", parameters.Select(param => param.functionNames.encodeFunctionName)) + " " + encodeParamName,
                        decodeExpression = jsonDecodeFunctionNamePrefix + jsonCodeDictFunctionNameCommonPart + " " + String.Join(" ", parameters.Select(param => param.functionNames.decodeFunctionName)),

                        dependencies = parameters.ToImmutableDictionary(
                            parameter => parameter.dependencies.canonicalTypeText,
                            parameter => parameter.dependencies),

                        referencedModules = parameters.SelectMany(parameter => parameter.dependencies.referencedModules).ToImmutableHashSet()
                    };
                }

                if (rootType.Instance.Value.genericType == "Result")
                {
                    if (parameters.Count != 2)
                        throw new Exception("Unexpected number of parameters for 'Result': got " + parameters.Count + " instead of 2.");

                    return new CompileSerializingExpressionsResult
                    {
                        canonicalTypeText = "Result " + String.Join(" ", parameters.Select(param => param.dependencies.canonicalTypeText)),

                        encodeExpression = jsonEncodeFunctionNamePrefix + jsonCodeResultFunctionNameCommonPart + " " + String.Join(" ", parameters.Select(param => param.functionNames.encodeFunctionName)) + " " + encodeParamName,
                        decodeExpression = jsonDecodeFunctionNamePrefix + jsonCodeResultFunctionNameCommonPart + " " + String.Join(" ", parameters.Select(param => param.functionNames.decodeFunctionName)),

                        dependencies = parameters.ToImmutableDictionary(
                            parameter => parameter.dependencies.canonicalTypeText,
                            parameter => parameter.dependencies),

                        referencedModules = parameters.SelectMany(parameter => parameter.dependencies.referencedModules).ToImmutableHashSet()
                    };
                }

                throw new NotImplementedException("Instantation is not implemented yet. (The type is '" + rootTypeText + "').");
            }

            if (rootType.Record != null)
            {
                Console.WriteLine("'" + rootTypeText + "' is a record type.");

                var fields = rootType.Record.Value.fields.Select(field =>
                    {
                        var fieldType = field.Value;

                        var fieldTypeExpressions = CompileSerializingExpressions(
                            fieldType,
                            sourceModuleName,
                            getModuleText);

                        var fieldFunctionNames = GetFunctionNamesFromTypeText(fieldTypeExpressions.canonicalTypeText);

                        var encodeFieldValueExpression =
                            encodeParamName + "." + field.Key + " |> " + fieldFunctionNames.encodeFunctionName;

                        var dependencies =
                            fieldTypeExpressions.dependencies
                            .SetItem(fieldTypeExpressions.canonicalTypeText, fieldTypeExpressions);

                        return new
                        {
                            fieldType,
                            encodeExpression = "( \"" + field.Key + "\", " + encodeFieldValueExpression + " )",
                            decodeExpression = "( Json.Decode.field \"" + field.Key + "\" " + fieldFunctionNames.decodeFunctionName + " )",
                            dependencies = dependencies,
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
                    .Aggregate((a, b) => a.SetItems(b));

                var encodeExpression =
                    "Json.Encode.object\n" +
                    IndentElmCodeLines(1, encodeListExpression);

                var recordFieldsNames = rootType.Record.Value.fields.Keys;

                var decodeMapFunction =
                    "(\\" + String.Join(" ", recordFieldsNames) + " -> { " +
                    String.Join(", ", recordFieldsNames.Select(fieldName => fieldName + " = " + fieldName))
                    + " })";

                var decodeExpression =
                    "Json.Decode.map" + (1 < fields.Count ? fields.Count.ToString() : "") + " " + decodeMapFunction + "\n" +
                    IndentElmCodeLines(1, dencodeListExpression);

                return new CompileSerializingExpressionsResult
                {
                    canonicalTypeText = sourceModuleName + "." + rootTypeText,
                    encodeExpression = encodeExpression,
                    decodeExpression = decodeExpression,
                    dependencies = allFieldsDependencies,
                    referencedModules = ImmutableHashSet<string>.Empty.Add(sourceModuleName),
                };
            }

            if (rootType.Custom != null)
            {
                throw new NotImplementedException("Custom type is not implemented yet.");
            }

            throw new Exception("Parsed invalid case for type '" + rootTypeText + "'");
        }


        static string ElmCodeIndentString(int level) =>
            level <= 0 ? "" : "    " + ElmCodeIndentString(level - 1);

        static string IndentElmCodeLines(int level, string textBeforeIndent)
        {
            var indentString = ElmCodeIndentString(level);

            return indentString + textBeforeIndent.Replace("\n", "\n" + indentString);
        }

        static public ElmType ParseElmTypeText(
            string elmTypeText)
        {
            //  Assume: elm-format was applied.
            //  Assume: Record fields are all on their own line.
            //  Assume: Custom type tags are all on their own line.
            //  Assume: Generic type instances (e.g. `List Int`) are on a single line.

            try
            {
                var typeDefinitionTextLines = elmTypeText.Split(new char[] { '\n' });

                if (typeDefinitionTextLines.Length == 1)
                {
                    var instanceComponents = Regex.Split(typeDefinitionTextLines[0].Trim(), @"\s");

                    if (1 < instanceComponents.Length)
                    {
                        //  Cases like: `Maybe String`, `List Int`, `Dict.Dict Int String`.

                        return new ElmType
                        {
                            Instance = (instanceComponents[0], instanceComponents.Skip(1).ToImmutableList())
                        };
                    }
                }

                var isAlias = Regex.IsMatch(typeDefinitionTextLines[0], @"^type(\s+)alias");

                if (isAlias)
                {
                    var isRecord = Regex.IsMatch(typeDefinitionTextLines[1], @"^\s+\{");

                    if (!isRecord)
                    {
                        return new ElmType
                        {
                            Alias = typeDefinitionTextLines[1].Trim(),
                        };
                    }

                    var fieldsCandidatesLines =
                        typeDefinitionTextLines.Skip(1).ToImmutableList();

                    var fields =
                        fieldsCandidatesLines
                        .Select(line => Regex.Match(line, @"^\s+({|,)\s*([\w\d_]+)\s*\:(.+)"))
                        .Where(match => match.Success)
                        .Select(match =>
                        {
                            var fieldName = match.Groups[2].Value;

                            var fieldType = match.Groups[3].Value.Trim();

                            return (fieldName, fieldType);
                        })
                        .ToImmutableDictionary(field => field.fieldName, field => field.fieldType);
                    ;

                    return new ElmType
                    {
                        Record = new ElmType.RecordStructure
                        {
                            fields = fields
                        }
                    };
                }
                else
                {
                    var tags =
                        typeDefinitionTextLines.Skip(1)
                        .Select(tagLine =>
                        {
                            var overallMatch = Regex.Match(tagLine, @"^\s+(=|\|)\s*([\w\d_]+)(.*)$");

                            if (!overallMatch.Success)
                                throw new Exception("Failed to parse Custom Type tag name from '" + tagLine + "'");

                            var parametersText = overallMatch.Groups[3].Value.Trim();

                            if (parametersText.Contains('{') || parametersText.Contains('('))
                                throw new NotImplementedException("Type structures in custom type tag parameters are not supported yet. You can work around this limitation by using type aliases when describing the tag parameter.");

                            var tagParameters =
                                Regex.Split(parametersText, @"\s+")
                                .Select(parameterText => parameterText.Trim())
                                .ToImmutableList();

                            return (tagName: overallMatch.Groups[2].Value, tagParameters: (IImmutableList<string>)tagParameters);
                        })
                        .ToImmutableDictionary(tag => tag.tagName, tag => tag.tagParameters);

                    return new ElmType
                    {
                        Custom = new ElmType.CustomStructure
                        {
                            tags = tags
                        },
                    };
                }
            }
            catch (Exception e)
            {
                throw new Exception("Failed to parse type '" + elmTypeText + "'.", e);
            }
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

                        return null;
                    }

                    var exposedNames =
                        exposedAggregated
                        .Split(new[] { ',' })
                        .Select(commaSeparatedValue => commaSeparatedValue.Trim())
                        .ToImmutableList();

                    if (exposedNames.Contains(importedNameInModule))
                        return (import.Key, importedNameInModule);
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

            public string Alias;

            public (string genericType, IImmutableList<string> parameters)? Instance;

            public struct CustomStructure
            {
                public IImmutableDictionary<string, IImmutableList<string>> tags;
            }

            public struct RecordStructure
            {
                public IImmutableDictionary<string, string> fields;
            }
        }

        static public
            (IImmutableDictionary<string, (string encodeExpression, string decodeExpression)> expressions,
            IImmutableSet<string> referencedModules)
            GetAllExpressionsFromTreeTransitive(
            CompileSerializingExpressionsResult tree)
        {
            var children =
                tree.dependencies.Select(child => GetAllExpressionsFromTreeTransitive(child.Value)).ToImmutableList();

            var referencedModules =
                children.SelectMany(child => child.referencedModules)
                .Concat(tree.referencedModules)
                .ToImmutableHashSet();

            var expressions =
                children.SelectMany(child => child.expressions)
                .ToImmutableDictionary(epressionForType => epressionForType.Key, epressionForType => epressionForType.Value)
                .SetItem(tree.canonicalTypeText, (tree.encodeExpression, tree.decodeExpression));

            return (expressions, referencedModules);
        }

        static public IImmutableList<string> generalSupportingFunctionsTexts => new[]{
            jsonEncodeFunctionNamePrefix + jsonCodeMaybeFunctionNameCommonPart + $@" encoder =
    Maybe.map encoder >> Maybe.withDefault Json.Encode.null
",
            jsonDecodeFunctionNamePrefix + jsonCodeMaybeFunctionNameCommonPart + $@" =
    Json.Decode.nullable
",

            jsonEncodeFunctionNamePrefix + jsonCodeListFunctionNameCommonPart + $@" encoder =
    Json.Encode.list encoder
",
            jsonDecodeFunctionNamePrefix + jsonCodeListFunctionNameCommonPart + $@" decoder =
    Json.Decode.list decoder
",
            jsonEncodeFunctionNamePrefix + jsonCodeDictFunctionNameCommonPart + $@" encodeKey encodeValue =
    Dict.toList
        >> Json.Encode.list
            (\( key, value ) -> [ ( ""key"", key |> encodeKey ), ( ""value"", value |> encodeValue ) ] |> Json.Encode.object)
",
            jsonDecodeFunctionNamePrefix + jsonCodeDictFunctionNameCommonPart + $@" decodeKey decodeValue =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field ""key"" decodeKey)
        (Json.Decode.field ""value"" decodeValue)
        |> Json.Decode.list
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
"
        }.ToImmutableList();
    }
}