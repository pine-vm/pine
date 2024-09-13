using Pine.Core;
using Pine.Json;
using System;
using System.Collections.Generic;
using System.Text.Json;

namespace Pine.PineVM;

public class EncodePineExpressionAsJson
{
    public static string ToJsonString(Expression expression) =>
        JsonSerializer.Serialize(expression, jsonSerializerOptions);

    public static string ToJsonString(IReadOnlyList<Expression> expression) =>
        JsonSerializer.Serialize(expression, jsonSerializerOptions);

    public static Expression SingleFromJsonString(string json) =>
        JsonSerializer.Deserialize<Expression>(json, jsonSerializerOptions) ?? throw new Exception();

    public static IReadOnlyList<Expression> ListFromJsonString(string json) =>
        JsonSerializer.Deserialize<IReadOnlyList<Expression>>(json, jsonSerializerOptions) ?? throw new Exception();

    public static object SingleOrListFromJsonString(string json)
    {
        if (json.TrimStart().StartsWith('['))
        {
            return ListFromJsonString(json);
        }

        return SingleFromJsonString(json);
    }

    readonly static JsonSerializerOptions jsonSerializerOptions = BuildJsonSerializerOptions();

    public static JsonSerializerOptions BuildJsonSerializerOptions()
    {
        var options = new JsonSerializerOptions { MaxDepth = 1000 };

        options.Converters.Add(new JsonConverterForPineValue());

        return options;
    }
}
