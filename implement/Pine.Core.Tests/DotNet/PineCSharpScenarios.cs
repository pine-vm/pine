using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.DotNet;
using Pine.Core.Encodings;
using Pine.Core.Json;
using Pine.Core.PineLanguage;
using Pine.Core.Testing;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace Pine.Core.Tests.DotNet;

internal sealed record PineCSharpScenarioVariant(
    string Name,
    PineCSharpCompilationOptions Options,
    IReadOnlyList<PineValueClass> Specializations);

internal sealed record PineCSharpScenarioEvaluation(string Name, PineValue Environment);

internal sealed record PineCSharpScenario(
    string Directory,
    Expression Entry,
    IReadOnlyDictionary<string, Expression> Expressions,
    IReadOnlyDictionary<string, PineValue> Values,
    IReadOnlyList<PineCSharpScenarioVariant> Variants,
    IReadOnlyList<PineCSharpScenarioEvaluation> Evaluations)
{
    public PineCSharpScenarioVariant Variant(string name) => Variants.Single(item => item.Name == name);

    public PineCSharpScenarioEvaluation Evaluation(string name) => Evaluations.Single(item => item.Name == name);

    public string ExpectationDirectory(string variant) => Path.Combine(Directory, "expect", Variant(variant).Name);
}

internal static class PineCSharpScenarios
{
    private static readonly JsonSerializerOptions s_outputOptions =
        new()
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            WriteIndented = true,
            NewLine = "\n",
            MaxDepth = 1000,
            Converters = { new JsonConverterForPineValue() }
        };

    public static string CorpusDirectory =>
        Path.Combine(TestResultSummary.FindTestDataDirectory("PineCSharpCompiler"), "scenarios");

    public static IEnumerable<object[]> Discover()
    {
        var directories = System.IO.Directory.GetDirectories(CorpusDirectory).Order(StringComparer.Ordinal).ToArray();

        if (directories.Length == 0)
            throw new InvalidDataException("No C# compiler scenarios found in " + CorpusDirectory);

        foreach (var directory in directories)
        {
            var scenario = LoadDirectory(directory);

            foreach (var variant in scenario.Variants)
                yield return [Path.GetFileName(directory), variant.Name];
        }
    }

    public static PineCSharpScenario Load(string name)
    {
        ValidateName(name, "scenario name");
        return LoadDirectory(Path.Combine(CorpusDirectory, name));
    }

    public static PineCSharpScenario LoadDirectory(string directory)
    {
        var input = Path.Combine(directory, "input");
        using var programDocument = ReadDocument(Path.Combine(input, "program.json"));
        var program = programDocument.RootElement;
        var location = Path.Combine(input, "program.json") + ":$";
        Properties(program, location, "entry", "expressions", "values");
        var expressions = Object(Required(program, "expressions", location), location + ".expressions");
        var values = Object(Required(program, "values", location), location + ".values");
        var resolver = new Resolver(expressions, values, location);

        var entry =
            resolver.Expression(String(Required(program, "entry", location), location + ".entry"), location + ".entry");

        resolver.ResolveAll();

        var compilationsPath = Path.Combine(input, "compilations.json");
        using var compilationsDocument = ReadDocument(compilationsPath);
        var compilations = Object(compilationsDocument.RootElement, compilationsPath + ":$");
        var variants = new List<PineCSharpScenarioVariant>();

        foreach (var item in compilations.EnumerateObject().OrderBy(item => item.Name, StringComparer.Ordinal))
        {
            var variantLocation = compilationsPath + ":$." + item.Name;
            ValidateName(item.Name, variantLocation);

            Properties(
                item.Value,
                variantLocation,
                "enableCancellation",
                "includeGenericEntry",
                "className",
                "literalsClassName",
                "maximumFunctionCount",
                "containerNamespace",
                "specializations",
                "maximumInlineMatchDifferingPositions");

            var options =
                new PineCSharpCompilationOptions(
                    EnableCancellation: Boolean(item.Value, "enableCancellation", true, variantLocation),
                    IncludeGenericEntry: Boolean(item.Value, "includeGenericEntry", true, variantLocation),
                    ClassName: OptionalString(item.Value, "className", "CompiledPine", variantLocation),
                    LiteralsClassName: OptionalString(item.Value, "literalsClassName", "PineLiterals", variantLocation),
                    MaximumFunctionCount:
                    item.Value.TryGetProperty("maximumFunctionCount", out var maximum)
                    ?
                    Integer(maximum, variantLocation + ".maximumFunctionCount", minimum: 1)
                    :
                    1024,
                    ContainerNamespace:
                    item.Value.TryGetProperty("containerNamespace", out var containerNamespace)
                    ?
                    String(containerNamespace, variantLocation + ".containerNamespace")
                    :
                    null,
                    MaximumInlineMatchDifferingPositions:
                    item.Value.TryGetProperty("maximumInlineMatchDifferingPositions", out var matchLimit)
                    ?
                    Integer(matchLimit, variantLocation + ".maximumInlineMatchDifferingPositions", minimum: 0)
                    :
                    4);

            var specializations = new List<PineValueClass>();

            if (item.Value.TryGetProperty("specializations", out var constraints))
            {
                foreach (var (constraint, index) in Items(constraints, variantLocation + ".specializations").Select((value, index) => (value, index)))
                {
                    var constraintLocation = variantLocation + ".specializations[" + index + "]";
                    var facts = new List<KeyValuePair<IReadOnlyList<int>, PineValue>>();
                    var paths = new HashSet<string>(StringComparer.Ordinal);

                    foreach (var (fact, factIndex) in Items(constraint, constraintLocation).Select((value, index) => (value, index)))
                    {
                        var factLocation = constraintLocation + "[" + factIndex + "]";
                        Properties(fact, factLocation, "path", "value");
                        var path = ReadPath(Required(fact, "path", factLocation), factLocation + ".path");

                        if (!paths.Add(string.Join(",", path)))
                            throw Error(factLocation + ".path", "Duplicate constraint path.");

                        facts.Add(
                            new(
                                path,
                                resolver.Value(Required(fact, "value", factLocation), factLocation + ".value")));
                    }

                    specializations.Add(PineValueClass.Create(facts));
                }
            }

            if (!options.IncludeGenericEntry && specializations.Count == 0)
            {
                throw Error(
                    variantLocation,
                    "A compilation must request a generic entry or at least one specialization.");
            }

            variants.Add(new(item.Name, options, specializations));
        }

        if (variants.Count == 0)
            throw Error(compilationsPath + ":$", "Expected at least one compilation variant.");

        var evaluationsPath = Path.Combine(input, "evaluations.json");
        using var evaluationsDocument = ReadDocument(evaluationsPath);
        var evaluations = new List<PineCSharpScenarioEvaluation>();
        var evaluationNames = new HashSet<string>(StringComparer.Ordinal);

        foreach (var (item, index) in Items(evaluationsDocument.RootElement, evaluationsPath + ":$").Select((value, index) => (value, index)))
        {
            var evaluationLocation = evaluationsPath + ":$[" + index + "]";
            Properties(item, evaluationLocation, "name", "environment");
            var name = String(Required(item, "name", evaluationLocation), evaluationLocation + ".name");

            if (!evaluationNames.Add(name))
                throw Error(evaluationLocation + ".name", "Duplicate evaluation name: " + name);

            evaluations.Add(
                new(
                    name,
                    resolver.Value(
                        Required(item, "environment", evaluationLocation),
                        evaluationLocation + ".environment")));
        }

        var expect = Path.Combine(directory, "expect");

        if (System.IO.Directory.Exists(expect))
        {
            var extraVariants =
                System.IO.Directory.GetDirectories(expect).Select(Path.GetFileName)
                .Except(variants.Select(variant => variant.Name), StringComparer.Ordinal).ToArray();

            if (extraVariants.Length > 0)
            {
                throw Error(
                    expect,
                    "Expectations for unknown compilation variants: " + string.Join(", ", extraVariants));
            }
        }

        return new(directory, entry, resolver.Expressions, resolver.Values, variants, evaluations);
    }

    public static string Serialize(object value) => JsonSerializer.Serialize(value, s_outputOptions) + "\n";

    public static PineValue? ExpectedValue(PineCSharpScenario scenario, string variant, string evaluation)
    {
        var path = Path.Combine(scenario.ExpectationDirectory(variant), "evaluations.json");
        using var document = ReadDocument(path);

        var item =
            document.RootElement.EnumerateArray().Single(item => item.GetProperty("name").GetString() == evaluation);

        var value = item.GetProperty("value");

        return
            value.ValueKind == JsonValueKind.Null
            ?
            null
            :
            new Resolver(default, default, path + ":$").Value(value, path + ":$");
    }

    public static IReadOnlyList<string> CompareSnapshots(
        string directory, IReadOnlyDictionary<string, string> outputs, bool allowRecording = true)
    {
        var mismatches = new List<string>();
        var recording = allowRecording && Environment.GetEnvironmentVariable("PINE_UPDATE_CSHARP_SNAPSHOTS") == "1";
        System.IO.Directory.CreateDirectory(directory);

        var unexpected =
            System.IO.Directory.GetFiles(directory).Select(path => Path.GetFileName(path))
            .Where(name => !name.EndsWith(".actual", StringComparison.Ordinal))
            .Except(outputs.Keys, StringComparer.Ordinal).Order(StringComparer.Ordinal).ToArray();

        if (unexpected.Length > 0)
            mismatches.Add("Unexpected expectation files in " + directory + ": " + string.Join(", ", unexpected));

        foreach (var (name, output) in outputs)
        {
            var path = Path.Combine(directory, name);
            var actual = output.Replace("\r\n", "\n");
            var expected = File.Exists(path) ? File.ReadAllText(path).Replace("\r\n", "\n") : null;

            if (actual == expected)
            {
                File.Delete(path + ".actual");
                continue;
            }

            File.WriteAllText(path + ".actual", actual);

            if (recording)
            {
                File.WriteAllText(path, actual);
                File.Delete(path + ".actual");
            }
            else
            {
                mismatches.Add(
                    path + ": " +
                    (expected is null
                    ?
                    "Missing expectation."
                    :
                    TestResultSummary.DescribeStringDifference(expected, actual)) +
                    " Actual written to " +
                    path +
                    ".actual");
            }
        }

        return mismatches;
    }

    private sealed class Resolver(JsonElement expressions, JsonElement values, string programLocation)
    {
        public Dictionary<string, Expression> Expressions { get; } = new(StringComparer.Ordinal);

        public Dictionary<string, PineValue> Values { get; } = new(StringComparer.Ordinal);

        private readonly Dictionary<string, JsonNode> _normalizedExpressions = new(StringComparer.Ordinal);

        private readonly Dictionary<string, PineValue> _encodedExpressions = new(StringComparer.Ordinal);

        private readonly HashSet<string> _active = new(StringComparer.Ordinal);

        public void ResolveAll()
        {
            foreach (var item in expressions.EnumerateObject())
                Expression(item.Name, programLocation + ".expressions." + item.Name);

            foreach (var item in values.EnumerateObject())
                NamedValue(item.Name, programLocation + ".values." + item.Name);
        }

        public Expression Expression(string name, string referenceLocation)
        {
            if (Expressions.TryGetValue(name, out var result))
                return result;

            if (!expressions.TryGetProperty(name, out var definition))
                throw Error(referenceLocation, "Undefined expression reference: " + name);

            var key = "expression:" + name;

            if (!_active.Add(key))
                throw Error(referenceLocation, "Cyclic fixture reference: " + key);

            try
            {
                var location = programLocation + ".expressions." + name;
                var normalized = NormalizeExpression(definition, location);

                try
                {
                    result = EncodePineExpressionAsJson.SingleFromJsonString(normalized.ToJsonString(s_outputOptions));
                }
                catch (JsonException exception)
                {
                    throw new InvalidDataException(location + ": " + exception.Message, exception);
                }

                Expressions.Add(name, result);
                _normalizedExpressions.Add(name, normalized);
                return result;
            }
            finally
            {
                _active.Remove(key);
            }
        }

        private PineValue NamedValue(string name, string referenceLocation)
        {
            if (Values.TryGetValue(name, out var result))
                return result;

            if (!values.TryGetProperty(name, out var definition))
                throw Error(referenceLocation, "Undefined value reference: " + name);

            var key = "value:" + name;

            if (!_active.Add(key))
                throw Error(referenceLocation, "Cyclic fixture reference: " + key);

            try
            {
                result = Value(definition, programLocation + ".values." + name);
                Values.Add(name, result);
                return result;
            }
            finally
            {
                _active.Remove(key);
            }
        }

        public PineValue Value(JsonElement element, string location)
        {
            if (element.ValueKind == JsonValueKind.Array)
            {
                return
                    PineValue.List(
                        [
                        .. element.EnumerateArray().Select((item, index) => Value(item, location + "[" + index + "]"))
                        ]);
            }

            if (element.ValueKind == JsonValueKind.Number &&
                BigInteger.TryParse(
                    element.GetRawText(),
                    System.Globalization.NumberStyles.AllowLeadingSign,
                    System.Globalization.CultureInfo.InvariantCulture,
                    out var integer))
            {
                return IntegerEncoding.EncodeSignedInteger(integer);
            }

            var variant = SingleProperty(element, location);

            var text =
                String(
                    variant.Value,
                    location + "." + variant.Name,
                    allowEmpty: variant.Name is "AsBase64" or "BlobAsString");

            switch (variant.Name)
            {
                case "ValueRef":
                    return NamedValue(text, location);

                case "EncodedExpression":
                    if (!_encodedExpressions.TryGetValue(text, out var encoded))
                    {
                        encoded = ExpressionEncoding.EncodeExpressionAsValue(Expression(text, location));
                        _encodedExpressions.Add(text, encoded);
                    }

                    return encoded;

                case "BlobAsString":
                    return StringEncoding.ValueFromString(text);

                case "AsBase64":
                    try
                    {
                        return PineValue.Blob(Convert.FromBase64String(text));
                    }
                    catch (FormatException exception)
                    {
                        throw new InvalidDataException(location + ": Invalid base64 blob.", exception);
                    }

                default:
                    throw Error(location, "Unsupported Pine value form: " + variant.Name);
            }
        }

        private JsonNode NormalizeExpression(JsonElement element, string location)
        {
            var variant = SingleProperty(element, location);

            if (variant.Name == "ExpressionRef")
            {
                var name = String(variant.Value, location + ".ExpressionRef");
                Expression(name, location);
                return _normalizedExpressions[name].DeepClone();
            }

            if (variant.Name == "EnvPath")
            {
                return
                    ParseNode(EncodePineExpressionAsJson.ToJsonString(
                    ExpressionBuilder.BuildExpressionForPathInExpression(
                        ReadPath(variant.Value, location + ".EnvPath"), Core.PineLanguage.Expression.EnvironmentInstance)))!;
            }

            var arguments = Items(variant.Value, location + "." + variant.Name).ToArray();

            var count =
                variant.Name switch
                {
                    "Environment" => 0,
                    "Literal" or "Litral" or "List" => 1,
                    "Eval" or "ParseAndEval" or "Builtin" or "KernelApplication" or "Label" or "StringTag" => 2,
                    "Conditional" => 3,

                    _ =>
                    throw Error(location, "Unknown expression variant: " + variant.Name)
                };

            if (arguments.Length != count)
                throw Error(location, "Expected " + count + " arguments for " + variant.Name + ".");

            var normalized = new JsonArray();

            for (var index = 0; index < arguments.Length; index++)
            {
                var argumentLocation = location + "." + variant.Name + "[" + index + "]";

                if (variant.Name is "Literal" or "Litral" || variant.Name == "Label" && index == 0)
                {
                    normalized.Add(
                        ParseNode(
                            JsonSerializer.Serialize(Value(arguments[index], argumentLocation), s_outputOptions)));
                }
                else if (variant.Name == "List")
                {
                    var items = new JsonArray();

                    foreach (var (item, itemIndex) in Items(arguments[index], argumentLocation).Select((value, index) => (value, index)))
                        items.Add(NormalizeExpression(item, argumentLocation + "[" + itemIndex + "]"));

                    normalized.Add(items);
                }
                else if (index == 0 && variant.Name is "Builtin" or "KernelApplication" or "StringTag")
                {
                    var name = String(arguments[index], argumentLocation, allowEmpty: variant.Name == "StringTag");

                    if (variant.Name != "StringTag" &&
                        !Core.Runtime.InternalExpression.BuiltinFunctions.TryFromName(name, out _))
                    {
                        throw Error(argumentLocation, "Unknown Pine builtin function: " + name);
                    }

                    normalized.Add(name);
                }
                else
                    normalized.Add(NormalizeExpression(arguments[index], argumentLocation));
            }

            return new JsonObject { [variant.Name] = normalized };
        }
    }

    private static JsonNode? ParseNode(string text) =>
        JsonNode.Parse(text, documentOptions: new JsonDocumentOptions { MaxDepth = 1000 });

    private static JsonDocument ReadDocument(string path)
    {
        try
        {
            return JsonDocument.Parse(File.ReadAllText(path), new JsonDocumentOptions { MaxDepth = 1000 });
        }
        catch (JsonException exception)
        {
            throw new InvalidDataException(path + ": " + exception.Message, exception);
        }
    }

    private static JsonElement Object(JsonElement element, string location)
    {
        if (element.ValueKind != JsonValueKind.Object)
            throw Error(location, "Expected a JSON object.");

        var names = new HashSet<string>(StringComparer.Ordinal);

        foreach (var item in element.EnumerateObject())
        {
            if (!names.Add(item.Name))
                throw Error(location + "." + item.Name, "Duplicate property.");
        }

        return element;
    }

    private static void Properties(JsonElement element, string location, params string[] allowed)
    {
        foreach (var item in Object(element, location).EnumerateObject())
        {
            if (!allowed.Contains(item.Name, StringComparer.Ordinal))
                throw Error(location + "." + item.Name, "Unknown property.");
        }
    }

    private static JsonElement Required(JsonElement element, string name, string location) =>
        element.TryGetProperty(name, out var value)
        ?
        value
        :
        throw Error(location + "." + name, "Missing required property.");

    private static JsonProperty SingleProperty(JsonElement element, string location)
    {
        var properties = Object(element, location).EnumerateObject().ToArray();
        return properties.Length == 1 ? properties[0] : throw Error(location, "Expected exactly one variant property.");
    }

    private static IEnumerable<JsonElement> Items(JsonElement element, string location) =>
        element.ValueKind == JsonValueKind.Array
        ?
        element.EnumerateArray()
        :
        throw Error(location, "Expected a JSON array.");

    private static string String(JsonElement element, string location, bool allowEmpty = false) =>
        element.ValueKind == JsonValueKind.String && element.GetString() is { } value &&
        (allowEmpty || value.Length > 0)
        ?
        value
        :
        throw Error(location, "Expected " + (allowEmpty ? "a string." : "a nonempty string."));

    private static string OptionalString(JsonElement element, string name, string fallback, string location) =>
        element.TryGetProperty(name, out var value) ? String(value, location + "." + name) : fallback;

    private static bool Boolean(JsonElement element, string name, bool fallback, string location) =>
        !element.TryGetProperty(name, out var value)
        ?
        fallback
        :
        value.ValueKind switch
        {
            JsonValueKind.True => true,
            JsonValueKind.False => false,

            _ =>
            throw Error(location + "." + name, "Expected a JSON boolean.")
        };

    private static int Integer(JsonElement element, string location, int minimum = 0) =>
        element.ValueKind == JsonValueKind.Number && element.TryGetInt32(out var value) && value >= minimum
        ?
        value
        :
        throw Error(location, "Expected an integer at least " + minimum + ".");

    private static int[] ReadPath(JsonElement element, string location) =>
        [.. Items(element, location).Select((item, index) => Integer(item, location + "[" + index + "]"))];

    private static void ValidateName(string name, string location)
    {
        if (string.IsNullOrWhiteSpace(name) || name is "." or ".." ||
            name.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 || name.Contains('\\') || name.Contains('/'))
        {
            throw Error(location, "Expected a safe directory name.");
        }
    }

    private static InvalidDataException Error(string location, string message) => new(location + ": " + message);
}
