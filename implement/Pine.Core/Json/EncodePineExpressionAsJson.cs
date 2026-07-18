using Pine.Core.CommonEncodings;
using System.Collections.Generic;
using System.Text.Json;

namespace Pine.Core.Json;

/// <summary>
/// Encoding and decoding of <see cref="Expression"/>s to and from JSON.
/// These JSON encodings are used mostly for debugging and testing, as a form that is human-readable for the purpose of inspection.
/// For productive use, <see cref="Expression"/>s are usually encoded using <see cref="ExpressionEncoding"/>.
/// </summary>
public class EncodePineExpressionAsJson
{
    /// <summary>
    /// Serializes a single <see cref="Expression"/> to a JSON string.
    /// </summary>
    public static string ToJsonString(Expression expression) =>
        JsonSerializer.Serialize(expression, s_jsonSerializerOptions);

    /// <summary>
    /// Serializes a list of <see cref="Expression"/>s to a JSON string.
    /// </summary>
    /// <returns>
    /// JSON string with an array of JSON objects, each representing a <see cref="Expression"/>.
    /// </returns>
    public static string ToJsonString(IReadOnlyList<Expression> expression) =>
        JsonSerializer.Serialize(expression, s_jsonSerializerOptions);

    /// <summary>
    /// Expects a JSON string encoding a single <see cref="Expression"/>.
    /// <para>
    /// Decoding constructs the expression tree using the <c>Instance</c> factory methods on
    /// <see cref="Expression"/> (for example <see cref="Expression.LitralInst"/> and
    /// <see cref="Expression.ListInst"/>), so that reuse of common heap instances also happens
    /// on the JSON decoding path.
    /// </para>
    /// </summary>
    public static Expression SingleFromJsonString(string json)
    {
        using var document = JsonDocument.Parse(json, s_jsonDocumentOptions);

        return ParseExpressionFromJsonElement(document.RootElement);
    }

    /// <summary>
    /// Expects a JSON string encoding a list (JSON array) of <see cref="Expression"/>.
    /// <para>
    /// Decoding constructs the expression tree using the <c>Instance</c> factory methods on
    /// <see cref="Expression"/> (for example <see cref="Expression.LitralInst"/> and
    /// <see cref="Expression.ListInst"/>), so that reuse of common heap instances also happens
    /// on the JSON decoding path.
    /// </para>
    /// </summary>
    public static IReadOnlyList<Expression> ListFromJsonString(string json)
    {
        using var document = JsonDocument.Parse(json, s_jsonDocumentOptions);

        return ParseExpressionListFromJsonElement(document.RootElement);
    }

    private static Expression ParseExpressionFromJsonElement(JsonElement element)
    {
        if (element.ValueKind is not JsonValueKind.Object)
            throw new JsonException("Expected object, got " + element.ValueKind);

        JsonProperty variantProperty = default;
        var found = false;

        foreach (var property in element.EnumerateObject())
        {
            if (found)
                throw new JsonException("Expected exactly one property on expression object");

            variantProperty = property;
            found = true;
        }

        if (!found)
            throw new JsonException("Expected exactly one property on expression object");

        var variantName = variantProperty.Name;
        var arguments = variantProperty.Value;

        if (arguments.ValueKind is not JsonValueKind.Array)
            throw new JsonException("Expected array of arguments for variant " + variantName);

        return variantName switch
        {
            "Environment" =>
            Expression.EnvironmentInstance,

            "Litral" or "Literal" =>
            Expression.LitralInst(
                ParsePineValueFromJsonElement(arguments[0])),

            "List" =>
            Expression.ListInst(
                ParseExpressionListFromJsonElement(arguments[0])),

            "Eval" or "ParseAndEval" =>
            new Expression.Eval(
                encoded: ParseExpressionFromJsonElement(arguments[0]),
                environment: ParseExpressionFromJsonElement(arguments[1])),

            "Builtin" or "KernelApplication" =>
            Expression.BuiltinInst(
                function:
                arguments[0].GetString()
                ?? throw new JsonException("Expected function name string"),
                input: ParseExpressionFromJsonElement(arguments[1])),

            "Conditional" =>
            Expression.ConditionalInst(
                condition: ParseExpressionFromJsonElement(arguments[0]),
                falseBranch: ParseExpressionFromJsonElement(arguments[1]),
                trueBranch: ParseExpressionFromJsonElement(arguments[2])),

            "Label" =>
            new Expression.Label(
                labelValue:
                ParsePineValueFromJsonElement(arguments[0])
                ?? throw new JsonException("Expected label value"),
                tagged: ParseExpressionFromJsonElement(arguments[1])),

            "StringTag" =>
            new Expression.Label(
                tag:
                arguments[0].GetString()
                ?? throw new JsonException("Expected tag string"),
                tagged: ParseExpressionFromJsonElement(arguments[1])),

            _ =>
            throw new JsonException(
                "Unexpected expression variant: " + variantName),
        };
    }

    private static IReadOnlyList<Expression> ParseExpressionListFromJsonElement(JsonElement element)
    {
        if (element.ValueKind is not JsonValueKind.Array)
            throw new JsonException("Expected array of expressions, got " + element.ValueKind);

        var items = new List<Expression>(element.GetArrayLength());

        foreach (var item in element.EnumerateArray())
        {
            items.Add(ParseExpressionFromJsonElement(item));
        }

        return items;
    }

    private static PineValue ParsePineValueFromJsonElement(JsonElement element) =>
        element.Deserialize<PineValue>(s_jsonSerializerOptions)
        ?? throw new JsonException("Failed to parse PineValue");

    private readonly static JsonSerializerOptions s_jsonSerializerOptions = BuildJsonSerializerOptions();

    private readonly static JsonDocumentOptions s_jsonDocumentOptions =
        new()
        {
            MaxDepth = 1000
        };

    /// <summary>
    /// Configuration to use <see cref="JsonSerializer"/> with <see cref="Expression"/>s.
    /// </summary>
    public static JsonSerializerOptions BuildJsonSerializerOptions()
    {
        var options =
            new JsonSerializerOptions
            {
                MaxDepth = 1000,
                NewLine = "\n"
            };

        options.Converters.Add(new JsonConverterForPineValue());

        return options;
    }
}
