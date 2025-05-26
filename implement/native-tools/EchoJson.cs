using System;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace native_tools;

[JsonSerializable(typeof(JsonElement))]
public partial class EchoJsonSerializerContext : JsonSerializerContext
{
}

public class EchoJson
{
    public static int EchoJsonLoop()
    {
        while (true)
        {
            var lineText = Console.ReadLine()?.Trim();

            if (string.IsNullOrEmpty(lineText))
            {
                continue;
            }

            try
            {
                var json = JsonSerializer.Deserialize(lineText, EchoJsonSerializerContext.Default.JsonElement);

                Console.WriteLine(JsonSerializer.Serialize(json, EchoJsonSerializerContext.Default.JsonElement) + "\n");
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine("Failed with runtime exception:\n" + ex);
                return 1;
            }
        }
    }
}
