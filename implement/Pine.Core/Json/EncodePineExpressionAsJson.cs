using Pine.Core.CommonEncodings;
using System;
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
    /// </summary>
    public static Expression SingleFromJsonString(string json) =>
        JsonSerializer.Deserialize<Expression>(json, s_jsonSerializerOptions)
        ?? throw new Exception();

    /// <summary>
    /// Expects a JSON string encoding a list (JSON array) of <see cref="Expression"/>.
    /// </summary>
    public static IReadOnlyList<Expression> ListFromJsonString(string json) =>
        JsonSerializer.Deserialize<IReadOnlyList<Expression>>(json, s_jsonSerializerOptions)
        ?? throw new Exception();

    private readonly static JsonSerializerOptions s_jsonSerializerOptions = BuildJsonSerializerOptions();

    /// <summary>
    /// Configuration to use <see cref="JsonSerializer"/> with <see cref="Expression"/>s.
    /// </summary>
    public static JsonSerializerOptions BuildJsonSerializerOptions()
    {
        var options = new JsonSerializerOptions { MaxDepth = 1000 };

        options.Converters.Add(new JsonConverterForPineValue());

        return options;
    }
}
